-- Pretty Printing and Parsing

-- Prettifier for a text format defined as words that are separated by one or
-- more whitespaces, prettified to contain only one whitespace in between the
-- words ¯\_(ツ)_/¯
prettify :: String -> String
prettify = decode . encode
    where
        encode = words
        decode = unwords
