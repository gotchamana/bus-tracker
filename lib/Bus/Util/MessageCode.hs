{-# LANGUAGE OverloadedStrings #-}

module Bus.Util.MessageCode (
    errorApiInvalidCredentials,
    errorApiInvalidRequestParameters,
    errorApiInvalidRequestFormat,
    errorApiMissingResource,
    errorApiUnknownError,
    errorValidationTrimmed,
    errorValidationNotEmpty,
    errorValidationValidPath,
    errorValidationNetworkPort,
    errorValidationValidJsonSyntax,
    errorValidationRequiredJsonKey,
    errorValidationRequiredJsonArrayItem,
    errorValidationValidJsonType,
    errorValidationValidJsonInteger,
    errorValidationUnknownJsonError,
    errorValidationDuplicateUserAccount,
    errorValidationUnknownError,
) where

import Data.Text (Text)

errorApiInvalidCredentials :: Text
errorApiInvalidCredentials = "error.api.invalid-credentials"

errorApiInvalidRequestParameters :: Text
errorApiInvalidRequestParameters = "error.api.invalid-request-parameters"

errorApiInvalidRequestFormat :: Text
errorApiInvalidRequestFormat = "error.api.invalid-request-format"

errorApiMissingResource :: Text
errorApiMissingResource = "error.api.missing-resource"

errorApiUnknownError :: Text
errorApiUnknownError = "error.api.unknown-error"

errorValidationTrimmed :: Text
errorValidationTrimmed = "error.validation.trimmed"

errorValidationNotEmpty :: Text
errorValidationNotEmpty = "error.validation.not-empty"

errorValidationValidPath :: Text
errorValidationValidPath = "error.validation.valid-path"

errorValidationNetworkPort :: Text
errorValidationNetworkPort = "error.validation.network-port"

errorValidationValidJsonSyntax :: Text
errorValidationValidJsonSyntax = "error.validation.valid-json-syntax"

errorValidationRequiredJsonKey :: Text
errorValidationRequiredJsonKey = "error.validation.required-json-key"

errorValidationRequiredJsonArrayItem :: Text
errorValidationRequiredJsonArrayItem = "error.validation.required-json-array-item"

errorValidationValidJsonType :: Text
errorValidationValidJsonType = "error.validation.valid-json-type"

errorValidationValidJsonInteger :: Text
errorValidationValidJsonInteger = "error.validation.valid-json-integer"

errorValidationUnknownJsonError :: Text
errorValidationUnknownJsonError = "error.validation.unknown-json-error"

errorValidationDuplicateUserAccount :: Text
errorValidationDuplicateUserAccount = "error.validation.duplicate-user-account"

errorValidationUnknownError :: Text
errorValidationUnknownError = "error.validation.unknown-error"
