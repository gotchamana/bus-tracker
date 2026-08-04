{-# LANGUAGE OverloadedStrings #-}

module Bus.Util.MessageCode (
    -- * API error, specific for 'ApiException'

    -- | Names should follow the "adjective + noun" form
    errorApiInvalidCredentials,
    errorApiInvalidRequestParameters,
    errorApiInvalidRequestFormat,
    errorApiMissingResource,
    errorApiUnknownError,

    -- * Validation error

    -- ** Common errors
    errorValidationTrimmed,
    errorValidationNotEmpty,
    errorValidationLength,
    errorValidationValidPath,
    errorValidationNetworkPort,

    -- ** Aeson better errors
    errorValidationValidJsonSyntax,
    errorValidationRequiredJsonKey,
    errorValidationRequiredJsonArrayItem,
    errorValidationValidJsonType,
    errorValidationValidJsonInteger,
    errorValidationUnknownJsonError,

    -- ** Others
    errorValidationDuplicateUserAccount,
    errorValidationInvalidUserCredentials,
    errorValidationMissingRefreshToken,
    errorValidationUnknownError,
) where

import Data.Text (Text)

-- | The client's request doesn't have the valid authentication token,
-- e.g. no token, token expired or token revoked.
errorApiInvalidCredentials :: Text
errorApiInvalidCredentials = "error.api.invalid-credentials"

-- | The client's request contains invalid parameters or body, usually combined
-- with the following validation errors
errorApiInvalidRequestParameters :: Text
errorApiInvalidRequestParameters = "error.api.invalid-request-parameters"

-- | The some parts of client's request cannot be parse correctly,
-- e.g. header, url or body
errorApiInvalidRequestFormat :: Text
errorApiInvalidRequestFormat = "error.api.invalid-request-format"

-- | General "Not Found" error
errorApiMissingResource :: Text
errorApiMissingResource = "error.api.missing-resource"

-- | Used in uncategorized errors
errorApiUnknownError :: Text
errorApiUnknownError = "error.api.unknown-error"

-- | String is not trimmed
errorValidationTrimmed :: Text
errorValidationTrimmed = "error.validation.trimmed"

-- | String is empty
errorValidationNotEmpty :: Text
errorValidationNotEmpty = "error.validation.not-empty"

-- | String length must be between the specified range
errorValidationLength :: Text
errorValidationLength = "error.validation.length"

-- | File path is invalid
errorValidationValidPath :: Text
errorValidationValidPath = "error.validation.valid-path"

-- | Specified number is not a valid port
errorValidationNetworkPort :: Text
errorValidationNetworkPort = "error.validation.network-port"

-- | JSON parse error
errorValidationValidJsonSyntax :: Text
errorValidationValidJsonSyntax = "error.validation.valid-json-syntax"

-- | Absent JSON key
errorValidationRequiredJsonKey :: Text
errorValidationRequiredJsonKey = "error.validation.required-json-key"

-- | Absent JSON array item
errorValidationRequiredJsonArrayItem :: Text
errorValidationRequiredJsonArrayItem = "error.validation.required-json-array-item"

-- | Wrong JSON type
errorValidationValidJsonType :: Text
errorValidationValidJsonType = "error.validation.valid-json-type"

-- | Wrong JSON integer type
errorValidationValidJsonInteger :: Text
errorValidationValidJsonInteger = "error.validation.valid-json-integer"

-- | Unknown JSON parse error
errorValidationUnknownJsonError :: Text
errorValidationUnknownJsonError = "error.validation.unknown-json-error"

-- | Register already existing account
errorValidationDuplicateUserAccount :: Text
errorValidationDuplicateUserAccount = "error.validation.duplicate-user-account"

-- | Wrong username or password
errorValidationInvalidUserCredentials :: Text
errorValidationInvalidUserCredentials = "error.validation.invalid-user-credentials"

-- | No refresh token
errorValidationMissingRefreshToken :: Text
errorValidationMissingRefreshToken = "error.validation.missing-refresh-token"

-- | General unknown validation error
errorValidationUnknownError :: Text
errorValidationUnknownError = "error.validation.unknown-error"
