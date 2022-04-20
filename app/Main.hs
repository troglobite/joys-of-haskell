import Data.Char (isAlphaNum, isSpace)


main :: IO ()
main =
  do
    putStrLn "Please Enter a Password"
    password <- Password <$> getLine
    print (validatePassword password)

checkLength :: Int -> String -> Either Error String
checkLength i s =
  case (length s > i) of
    True -> Left ( Error ((show s) ++ "cannot be longer then " ++ (show i)) )
    False -> Right s

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case length password > 20 of
    True -> Left ( "Your password cannot be longer than 20 characters.")
    False -> Right ( password )

checkUsernameLength :: String -> Either String String
checkUsernameLength name =
  case (length name > 15 ) of
    True -> Left ( "Username cannot be longer than 15 characters" )
    False -> Right ( name )

requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left ( "Cannot contain whitespace or special characters" )
    True -> Right xs

cleanWhiteSpace :: String -> Either String String
cleanWhiteSpace "" = Left ("Cannot be empty")
cleanWhiteSpace (x : xs) =
  case isSpace x of
    True -> cleanWhiteSpace xs
    False -> Right (x : xs)

validatePassword :: Password -> Either String String
validatePassword (Password password) =
  -- Implement Maybe Monad
  cleanWhiteSpace password
    >>= requireAlphaNum
    -- >>= Password <$> checkLength 20
    >>= checkPasswordLength

validateUsername :: Username -> Either String String
validateUsername (Username username) =
  cleanWhiteSpace username
    >>= requireAlphaNum
    >>= checkUsernameLength

reverseLine :: IO ()
reverseLine = 
  do
    word <- getLine 
    print ( reverse word )

bindMaybe :: Maybe a -> ( a -> Maybe b ) -> Maybe b
bindMaybe a b = case a of
  Nothing -> Nothing
  Just a' -> b a'

data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> ( a -> StringOrValue b ) -> StringOrValue b
bindStringOrValue a b = 
  case a of
    Val a' -> b a'
    Str string -> Str string

newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show

printTestResult :: Either String () -> IO ()
printTestResult r =
    case r of
      Left err -> putStrLn err
      Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
    case (actual == expected) of
        True -> Right ()
        False -> Left (unlines
            [ "Test " ++ show n
            , "  Expected:  " ++ show expected
            , "  But got:   " ++ show actual
            ])

test :: IO ()
test = printTestResult $
    do
       eq 1 ( checkPasswordLength "") (Right "")
       eq 2 ( checkPasswordLength "julielovesbooks") (Right "julielovesbooks")
       eq 3 ( checkPasswordLength "password longer then 20 characters long i hope") (Left "Your password cannot be longer than 20 characters.")
       eq 4 ( cleanWhiteSpace "") (Left "Cannot be empty")
       eq 5 ( cleanWhiteSpace "  password") (Right "password")
       eq 6 ( cleanWhiteSpace "        ") ( Left "Cannot be empty")
       eq 7 ( requireAlphaNum " &#^&@%#password" ) ( Left "Cannot contain whitespace or special characters")
       eq 8 ( requireAlphaNum "password1234" ) ( Right "password1234") 
