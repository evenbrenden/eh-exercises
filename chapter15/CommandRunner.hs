{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CommandRunner where

import Data.Kind
import Data.Maybe
import Data.Proxy
import GHC.TypeLits
import ShellCommandGADTs

data CommandSet :: [Symbol] -> [Type] -> Type where
    EmptyCommandSet :: CommandSet '[] '[]
    AddCommand ::
        KnownSymbol name =>
        ShellCmd a b ->
        CommandSet names commands ->
        CommandSet (name : names) (ShellCmd a b : commands)

commands =
    AddCommand @"ls" listDirectory $
        addLiteral @"free" "free -h" $
            addLiteral @"uptime" "uptime" $
                addLiteral @"uname" "uname -a" $
                    addLiteral @"system info" "neofetch" EmptyCommandSet
    where
        addLiteral ::
            forall name {names} {commands}.
            KnownSymbol name =>
            String ->
            CommandSet names commands ->
            CommandSet (name : names) (ShellCmd () String : commands)
        addLiteral command = AddCommand (literal command)
        literal :: String -> ShellCmd () String
        literal shellCommand =
            RunCommand (ProgName "bash") args outputFunc
            where
                args = const $ ProgArgs ["-c", shellCommand]
                outputFunc = const id

class
    CommandByName'
        (matches :: Bool)
        (name :: Symbol)
        commands
        shellIn
        shellOut
        | name commands -> shellIn shellOut
    where
    lookupProcessByName' :: proxy1 matches -> proxy2 name -> commands -> ShellCmd shellIn shellOut

instance CommandByName' True name (CommandSet (name : names) (ShellCmd a b : types)) a b where
    lookupProcessByName' _ _ (AddCommand cmd _) = cmd

instance
    ( nextMatches ~ HeadMatches name names,
      CommandByName' nextMatches name (CommandSet names types) shellIn shellOut
    ) =>
    CommandByName' False name (CommandSet (badName : names) (t : types)) shellIn shellOut
    where
    lookupProcessByName' _ nameProxy (AddCommand _ rest) =
        lookupProcessByName' (Proxy @nextMatches) nameProxy rest

type family HeadMatches (name :: Symbol) (names :: [Symbol]) :: Bool where
    HeadMatches name (name : _) = True
    HeadMatches name _ = False

class CommandByName (name :: Symbol) commands shellIn shellOut | commands name -> shellIn shellOut where
    lookupProcessByName ::
        proxy name -> commands -> ShellCmd shellIn shellOut

instance
    ( matches ~ HeadMatches name names,
      HasMatch name names,
      CommandByName' matches name (CommandSet names types) shellIn shellOut
    ) =>
    CommandByName name (CommandSet names types) shellIn shellOut
    where
    lookupProcessByName _ =
        lookupProcessByName' (Proxy @matches) (Proxy @name)

type HasMatch a as = HasMatch' a as as

type family
    HasMatch'
        (needles :: Symbol)
        (haystack :: [Symbol])
        (ctx :: [Symbol]) ::
        Constraint
    where
    HasMatch' a '[] ctx =
        TypeError
            ( Text "Command '"
                :<>: Text a
                :<>: Text "' is not in the command list: "
                :<>: ShowType ctx
            )
    HasMatch' a (a : as) ctx = ()
    HasMatch' a (b : as) ctx = HasMatch' a as ctx

runNamedCommand ::
    forall name {commands} {shellIn} {shellOut}.
    ( KnownSymbol name,
      CommandByName name commands shellIn shellOut
    ) =>
    commands ->
    shellIn ->
    IO shellOut
runNamedCommand allowedCommands input =
    let process = lookupProcessByName (Proxy @name) allowedCommands
     in runShellCmd process input

runScript availableProcesses = do
    runNamedCommand @"ls" availableProcesses "." >>= mapM_ putStrLn
    runNamedCommand @"free" availableProcesses () >>= putStr
    runNamedCommand @"uname" availableProcesses () >>= putStr

-- Classless runScript PASS
