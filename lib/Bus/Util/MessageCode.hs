{-# LANGUAGE OverloadedStrings #-}

module Bus.Util.MessageCode (
    errorApiInvalidCredentials,
    errorApiInvalidRequestParameters,
    errorApiInvalidRequestFormat,
    errorApiUnknownError,
    errorValidationTrimmed,
    errorValidationNotEmpty,
    errorValidationValidPath,
    errorValidationNetworkPort,
) where

import Data.Text (Text)

errorApiInvalidCredentials :: Text
errorApiInvalidCredentials = "error.api.invalid-credentials"

errorApiInvalidRequestParameters :: Text
errorApiInvalidRequestParameters = "error.api.invalid-request-parameters"

errorApiInvalidRequestFormat :: Text
errorApiInvalidRequestFormat = "error.api.invalid-request-format"

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
