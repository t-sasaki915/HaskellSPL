module Language.SPL.Parser.Internal
    ( ShakespeareCharacter
    , shakespeareCharacters
    , untilPeriod
    , manyTill1
    , space'
    , newline
    ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as Text

type ShakespeareCharacter = Text

shakespeareCharacters :: [ShakespeareCharacter]
shakespeareCharacters =
    [ "Achilles"            , "Adonis"                         , "Adriana"             , "Aegeon"               , "Aemilia"
    , "Agamemnon"           , "Agrippa"                        , "Ajax"                , "Alonso"               , "Andromache"
    , "Angelo"              , "Antiochus"                      , "Antonio"             , "Arthur"               , "Autolycus"
    , "Balthazar"           , "Banquo"                         , "Beatrice"            , "Benedick"             , "Benvolio"
    , "Bianca"              , "Brabantio"                      , "Brutus"              , "Capulet"              , "Cassandra"
    , "Cassius"             , "Christopher Sly"                , "Cicero"              , "Claudio"              , "Claudius"
    , "Cleopatra"           , "Cordelia"                       , "Cornelius"           , "Cressida"             , "Cymberline"
    , "Demetrius"           , "Desdemona"                      , "Dionyza"             , "Doctor Caius"         , "Dogberry"
    , "Don John"            , "Don Pedro"                      , "Donalbain"           , "Dorcas"               , "Duncan"
    , "Egeus"               , "Emilia"                         , "Escalus"             , "Falstaff"             , "Fenton"
    , "Ferdinand"           , "Ford"                           , "Fortinbras"          , "Francisca"            , "Friar John"
    , "Friar Laurence"      , "Gertrude"                       , "Goneril"             , "Hamlet"               , "Hecate"
    , "Hector"              , "Helen"                          , "Helena"              , "Hermia"               , "Hermonie"
    , "Hippolyta"           , "Horatio"                        , "Imogen"              , "Isabella"             , "John of Gaunt"
    , "John of Lancaster"   , "Julia"                          , "Juliet"              , "Julius Caesar"        , "King Henry"
    , "King John"           , "King Lear"                      , "King Richard"        , "Lady Capulet"         , "Lady Macbeth"
    , "Lady Macduff"        , "Lady Montague"                  , "Lennox"              , "Leonato"              , "Luciana"
    , "Lucio"               , "Lychorida"                      , "Lysander"            , "Macbeth"              , "Macduff"
    , "Malcolm"             , "Mariana"                        , "Mark Antony"         , "Mercutio"             , "Miranda"
    , "Mistress Ford"       , "Mistress Overdone"              , "Mistress Page"       , "Montague"             , "Mopsa"
    , "Oberon"              , "Octavia"                        , "Octavius Caesar"     , "Olivia"               , "Ophelia"
    , "Orlando"             , "Orsino"                         , "Othello"             , "Page"                 , "Pantino"
    , "Paris"               , "Pericles"                       , "Pinch"               , "Polonius"             , "Pompeius"
    , "Portia"              , "Priam"                          , "Prince Henry"        , "Prospero"             , "Proteus"
    , "Publius"             , "Puck"                           , "Queen Elinor"        , "Regan"                , "Robin"
    , "Romeo"               , "Rosalind"                       , "Sebastian"           , "Shallow"              , "Shylock"
    , "Slender"             , "Solinus"                        , "Stephano"            , "Thaisa"               , "The Abbot of Westminster"
    , "The Apothecary"      , "The Archbishop of Canterbury"   , "The Duke of Milan"   , "The Duke of Venice"   , "The Ghost"
    , "Theseus"             , "Thurio"                         , "Timon"               , "Titania"              , "Titus"
    , "Troilus"             , "Tybalt"                         , "Ulysses"             , "Valentine"            , "Venus"
    , "Vincentio"           , "Viola"
    ]

untilPeriod :: Parser Text
untilPeriod = Text.pack <$> manyTill1 anyChar (char '.')

manyTill1 :: Parser a -> Parser b -> Parser [a]
manyTill1 p end = (:) <$> p <*> manyTill p end

space' :: Parser Char
space' = char ' ' <|> char '\t'

newline :: Parser Char
newline = char '\n'
