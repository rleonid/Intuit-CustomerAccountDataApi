{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Signing (
  assertionWithSignature
  , CommonArguments (..)
  )
  where

import Codec.Crypto.RSA (hashSHA1, rsassa_pkcs1_v1_5_sign)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey(..))
import Crypto.Types.PubKey.RSA (PrivateKey)

-- All of the signing, hashing and encoding packages use Lazy ByteStrings
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Base64.Lazy as B64 (encode)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)

import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import Text.InterpolatedString.Perl6 (qc)

-- common set of arguments necessary in the signing functions.
data CommonArguments = C { referenceId :: ByteString
                         , userId      :: ByteString
                         , samlId      :: ByteString
                         , now         :: UTCTime
                         }
                       deriving (Show)

-- Unfortunately, we have to keep the XML condensed so that it passes Intuit's
-- checks. Don't bother rewriting this in a more readable style.

-------------------------------
-- XmlStructures
-------------------------------

-- Create an assertion about the user
assertionXml :: CommonArguments -> ByteString -> ByteString
assertionXml (C referenceId userId samlId now) signatureXml =
    [qc|<saml2:Assertion xmlns:saml2="urn:oasis:names:tc:SAML:2.0:assertion" ID="_{referenceId}" IssueInstant="{nowStr}" Version="2.0"><saml2:Issuer>{samlId}</saml2:Issuer>{signatureXml} <saml2:Subject><saml2:NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified">{userId}</saml2:NameID><saml2:SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer"></saml2:SubjectConfirmation></saml2:Subject><saml2:Conditions NotBefore="{beforeStr}" NotOnOrAfter="{afterStr}"><saml2:AudienceRestriction><saml2:Audience>{samlId}</saml2:Audience></saml2:AudienceRestriction></saml2:Conditions><saml2:AuthnStatement AuthnInstant="{nowStr}" SessionIndex="_{referenceId}"><saml2:AuthnContext><saml2:AuthnContextClassRef>urn:oasis:names:tc:SAML:2.0:ac:classes:unspecified</saml2:AuthnContextClassRef></saml2:AuthnContext></saml2:AuthnStatement></saml2:Assertion>|]
  where nowStr    = formatISO8601Millis now
        beforeStr = formatISO8601Millis $ addUTCTime ((-5) * 60) now
        afterStr  = formatISO8601Millis $ addUTCTime (10 * 60) now

-- Include a hash of the assertion
signedInfoXml :: CommonArguments -> ByteString
signedInfoXml c = 
  [qc|<ds:SignedInfo xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"></ds:CanonicalizationMethod><ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"></ds:SignatureMethod><ds:Reference URI="#_{reference}"><ds:Transforms><ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"></ds:Transform><ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"></ds:Transform></ds:Transforms><ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"></ds:DigestMethod><ds:DigestValue>{digestStr}</ds:DigestValue></ds:Reference></ds:SignedInfo>|]
  where reference = referenceId c
        digestStr = assertionXmlDigest c

-- Signed, and encoded, Assertion
keySigned :: CommonArguments -> PrivateKey -> ByteString
keySigned c key = signWithKey key $ signedInfoXml c

-- Full Signature
signatureXml :: CommonArguments -> PrivateKey -> ByteString
signatureXml c key = 
  [qc|<ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/><ds:SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/><ds:Reference URI="#_{referenceStr}"><ds:Transforms><ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/><ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/></ds:Transforms><ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/><ds:DigestValue>{digestStr}</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>{signatureStr}</ds:SignatureValue></ds:Signature>|]
  where referenceStr  = referenceId c
        digestStr     = assertionXmlDigest c
        signatureStr  = keySigned c key

-- Full Method
assertionWithSignature :: CommonArguments -> PrivateKey -> ByteString
assertionWithSignature c key = B64.encode assertionStr
  where assertionStr = assertionXml c $ signatureXml c key

-------------------------------
-- Signing and Transforming
-------------------------------

-- Compute a Base64 encoded Sha1 hash
digest :: ByteString -> ByteString
digest = B64.encode . bytestringDigest . sha1

-- Compute the hash of the assertion
assertionXmlDigest :: CommonArguments -> ByteString
assertionXmlDigest c = digest unsigned
  where unsigned = assertionXml c ""

signWithKey :: PrivateKey -> ByteString -> ByteString
signWithKey key m = B64.encode $ rsassa_pkcs1_v1_5_sign hashSHA1 key m

