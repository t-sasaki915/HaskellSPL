module Language.SPL.Parser
    ( SPLProgram (..)
    , Act (..)
    , Scene (..)
    , title
    , variableDeclaration
    , shakespeareCharacter
    ) where

import           Data.Attoparsec.Text
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.SPL.Parser.Internal

data SPLProgram = SPLProgram Text [ShakespeareCharacter] [Act]

data Act = Act RomanNumber [Scene]

data Scene = Scene RomanNumber

type RomanNumber = Text

title :: Parser Text
title = untilPeriod

variableDeclaration :: Parser Text
variableDeclaration = shakespeareCharacter <* string ", " <* untilPeriod

shakespeareCharacter :: Parser Text
shakespeareCharacter = choice (map string shakespeareCharacters) <?> "Shakespeare character"

act :: Parser Act
act = do
    _         <- skipMany space'
    _         <- string "Act "
    actNumber <- romanNumber
    _         <- string ": "
    _         <- untilPeriod
    _         <- skipMany newline
    scenes    <- many1 (scene <* skipMany newline)

    pure (Act actNumber scenes)

scene :: Parser Scene
scene = do
    _           <- skipMany space'
    _           <- string "Scene "
    sceneNumber <- romanNumber
    _           <- string ": "
    _           <- untilPeriod

    pure (Scene sceneNumber)

romanNumber :: Parser RomanNumber
romanNumber = Text.pack <$> many1 (choice (map char "IVXLCDM")) <?> "Roman number"
