{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:     Network.FastIRC.Messages
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- Parser and printer for IRC messages.

module Network.FastIRC.Messages
  ( -- * IRC messages
    Message(..),
    messageParser,
    readMessage,
    showMessage,

    -- * IRC commands
    Command(..),
    commandParser,
    showCommand
  )
  where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8 as P hiding (many)
import Data.Char
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Network.FastIRC.Types
import Network.FastIRC.Users
import Network.FastIRC.Utils
import Text.Printf


-- | Data type for IRC messages.

data Message =
  Message {
    msgOrigin  :: !(Maybe UserSpec), -- ^ Message origin (user/server).
    msgCommand :: !Command           -- ^ Message command or numeric.
  }
  deriving (Eq, Read, Show)


-- | Data type for IRC commands.

data Command
  = StringCmd CommandName [CommandArg]  -- ^ Arbitrary string command.
  | NumericCmd Integer [CommandArg]     -- ^ Arbitrary numeric command.

  | JoinCmd     (Map ChannelName (Maybe ChannelKey))
  | KickCmd     (Set ChannelName) (Set NickName) (Maybe CommandArg)
  | ModeCmd     (Maybe (TargetName, CommandArg, [CommandArg]))
  | NickCmd     NickName (Maybe Int)
  | NoticeCmd   (Set TargetName) CommandArg
  | PartCmd     (Set ChannelName) (Maybe CommandArg)
  | PassCmd     CommandArg
  | PingCmd     ServerName (Maybe ServerName)
  | PongCmd     ServerName (Maybe ServerName)
  | PrivMsgCmd  (Set TargetName) CommandArg
  | QuitCmd     (Maybe CommandArg)
  | TopicCmd    ChannelName (Maybe CommandArg)
  | UserCmd     UserName CommandArg CommandArg CommandArg

  deriving (Eq, Read, Show)


-- | Parser for IRC commands and their arguments.

commandParser :: Parser Command
commandParser =
  try numCmd <|>
  stringCmd

  where
    cmdArg :: Parser CommandArg
    cmdArg = do
      skipMany1 (char ' ')
      try lastArg <|> takeWhile1 isIRCTokChar

      where
        lastArg :: Parser CommandArg
        lastArg = char ':' *> P.takeWhile isMessageChar

    commaArg :: Parser (Set CommandArg)
    commaArg = S.filter (not . B.null) . S.fromList . B.split ',' <$> cmdArg

    intArg :: Parser (Maybe Int)
    intArg = option Nothing (fmap fst . B.readInt <$> cmdArg)

    joinCmd :: Parser Command
    joinCmd = do
      channels <- B.split ',' <$> cmdArg
      keys <- option [] $ B.split ',' <$> cmdArg
      many cmdArg
      return . JoinCmd . M.fromList $
        zip channels (map Just keys ++ repeat Nothing)

    numCmd :: Parser Command
    numCmd = NumericCmd <$> decimal <*> many cmdArg

    optArg :: Parser (Maybe CommandArg)
    optArg = option Nothing (Just <$> cmdArg)

    stringCmd :: Parser Command
    stringCmd = do
      cmd <- B.map toUpper <$> takeWhile1 isCommandChar
      case cmd of
        "JOIN" -> joinCmd
        "KICK" -> KickCmd <$> commaArg <*> commaArg <*> optArg <* many cmdArg
        "MODE" ->
          try ((\a b c -> ModeCmd (Just (a,b,c)))
               <$> cmdArg
               <*> cmdArg
               <*> many cmdArg)
          <|> (many cmdArg >>= guard . null >> pure (ModeCmd Nothing))
        "NICK" -> NickCmd <$> cmdArg <*> intArg <* many cmdArg
        "NOTICE" -> NoticeCmd <$> commaArg <*> cmdArg <* many cmdArg
        "PART" -> PartCmd <$> commaArg <*> optArg <* many cmdArg
        "PASS" -> PassCmd <$> cmdArg <* many cmdArg
        "PING" -> PingCmd <$> cmdArg <*> optArg <* many cmdArg
        "PONG" -> PongCmd <$> cmdArg <*> optArg <* many cmdArg
        "PRIVMSG" -> PrivMsgCmd <$> commaArg <*> cmdArg <* many cmdArg
        "QUIT" -> QuitCmd <$> optArg <* many cmdArg
        "TOPIC" -> TopicCmd <$> cmdArg <*> optArg <* many cmdArg
        "USER" -> UserCmd <$> cmdArg <*> cmdArg <*> cmdArg <*> cmdArg <* many cmdArg
        _      -> StringCmd cmd <$> many cmdArg


-- | Parser for IRC messages.

messageParser :: Parser Message
messageParser =
  Message <$> option Nothing (Just <$> try userSpec)
          <*> commandParser

  where
    userSpec :: Parser UserSpec
    userSpec = char ':' *> userParser <* skipMany1 (char ' ')


-- | Run the 'messageParser' parser.

readMessage :: MsgString -> Maybe Message
readMessage = parseComplete messageParser


-- | Turn a 'Command' into a 'B.ByteString'.  If you need to specify an
-- origin for the command, you should use 'Message' together with
-- 'showMessage'.

showCommand :: Command -> MsgString
showCommand cmd =
  case cmd of
    StringCmd cmdStr args  -> B.append cmdStr (showArgs args)
    NumericCmd cmdNum args ->
      B.append (B.pack . printf "%03i" $ cmdNum)
               (showArgs args)

    JoinCmd channels ->
      case formatJoins channels of
        (chanList, "")      -> "JOIN" +-+ [chanList]
        (chanList, keyList) -> "JOIN" +-+ [chanList, keyList]
    KickCmd channels nicks Nothing ->
      "KICK" +-+ [commaList channels, commaList nicks]
    KickCmd channels nicks (Just reason) ->
      "KICK" +-+ [commaList channels, commaList nicks, reason]
    ModeCmd Nothing          -> "MODE"
    ModeCmd (Just (target, mode, args)) ->
      "MODE" +-+ [target, mode] ++ args
    NickCmd nick (Just hc)   -> "NICK" +-+ [nick, B.pack (show hc)]
    NickCmd nick Nothing     -> "NICK" +-+ [nick]
    NoticeCmd targets text   -> "NOTICE" +-+ [commaList targets, text]
    PartCmd chans Nothing    -> "PART" +-+ [commaList chans]
    PartCmd chans (Just reason) ->
      "PART" +-+ [commaList chans, reason]
    PassCmd pwd              -> "PASS" +-+ [pwd]
    PingCmd srv1 Nothing     -> "PING" +-+ [srv1]
    PingCmd srv1 (Just srv2) -> "PING" +-+ [srv1, srv2]
    PongCmd srv1 Nothing     -> "PONG" +-+ [srv1]
    PongCmd srv1 (Just srv2) -> "PONG" +-+ [srv1, srv2]
    PrivMsgCmd targets text  -> "PRIVMSG" +-+ [commaList targets, text]
    QuitCmd Nothing          -> "QUIT" +-+ []
    QuitCmd (Just reason)    -> "QUIT" +-+ [reason]
    TopicCmd channel Nothing -> "TOPIC" +-+ [channel]
    TopicCmd channel (Just newTopic) ->
      "TOPIC" +-+ [channel, newTopic]
    UserCmd user vhost vport realName ->
      "USER" +-+ [user, vhost, vport, realName]

  where
    (+-+) :: B.ByteString -> [B.ByteString] -> B.ByteString
    cmd +-+ args = B.append cmd (showArgs args)
    infix 4 +-+

    formatJoins :: Map ChannelName (Maybe ChannelKey) ->
                   (CommandArg, CommandArg)
    formatJoins channels = (chanList, keyList)
      where
        (withKey, withoutKey) = M.partition isJust channels
        chanWithKeyAssocs = M.assocs withKey
        chanList = B.intercalate "," $ map fst chanWithKeyAssocs ++
                                       M.keys withoutKey
        keyList  = B.intercalate "," $ map (fromJust . snd) chanWithKeyAssocs

    commaList :: Set CommandArg -> CommandArg
    commaList = B.intercalate "," . S.toList

    showArgs :: [CommandArg] -> MsgString
    showArgs [] = B.empty
    showArgs [arg]
      | B.null arg        = " :"
      | B.head arg == ':' = B.append " :" arg
      | B.elem ' ' arg    = B.append " :" arg
      | otherwise         = B.cons ' ' arg
    showArgs (arg:args) =
      B.append (B.cons ' ' arg) (showArgs args)


-- | Turn a 'Message' into a 'B.ByteString'.

showMessage :: Message -> MsgString
showMessage (Message origin cmd) =
  case origin of
    Nothing -> showCommand cmd
    Just o  ->
      B.append (':' `B.cons` showUserSpec o)
               (' ' `B.cons` showCommand cmd)
