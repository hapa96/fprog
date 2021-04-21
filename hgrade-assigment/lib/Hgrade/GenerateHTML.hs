-- | The GenerateHTML Module renders the HTML for the Application. It returns the full Page in HTML representation.
module HGrade.GenerateHTML where

-- | define a data type and distiguish between all the different HTML Elements
data Html = P String | H1 String | Ul [Html] | Href String String | Table [Html] | Tr [Html] | Td [String] | Th [String] | Form [String] | Histogram [(Int, Int, Int)] deriving (Show)

-- | Render a HTML Page. Takes an Array of HTML Elements and returns a String, which represents the whole HTML Page
render :: [Html] -> String
render [] = []
render (x : xs) = "<link rel=\"stylesheet\" href=\"/static/styles.css\">" ++ addTag x ++ render xs

-- | Methode to Build the HTML Tags. Takes A Tag-String, Specigications and the actul Value, which will be placed within the tag
buildTag :: String -> String -> String -> String
buildTag tag specifications value = "<" ++ tag ++ " " ++ specifications ++ ">" ++ value ++ "</" ++ tag ++ ">"

-- | Helper Function for generating HTML Elements
addTag :: Html -> String
addTag (H1 x) = buildTag "h1" "" x
addTag (P x) = buildTag "p" "" x
addTag (Href link text) = buildTag "a" ("href=\"" ++ link ++ "\"") text
addTag (Ul x) = buildTag "ul" "" $ addListElement x
addTag (Table x) = buildTag "table" "style=\"width:60%\"" $ buildTable x
addTag (Tr x) = buildRow x
addTag (Td x) = buildTd x
addTag (Th x) = buildTh x
addTag (Form x) = buildTag "form" "action=\"/grade\" method=\"POST\"" (addFormElement x ++ buildTag "input" "type=\"submit\" value=\"Send\"" "")
addTag (Histogram x) = buildTag "tr" "" $ buildTag "td" "" $ "Histogram" ++ buildHistogram x

-- | Build the whole Histogram for every Topic. Takes a List of Tripples and returns the String, which can be rendered as HTML
buildHistogram :: [(Int, Int, Int)] -> String
buildHistogram = concatMap buildOneTopic

-- | Build a Histogram for one Topic
buildOneTopic :: (Int, Int, Int) -> String
buildOneTopic (x, y, z) = buildTag "td" "" $ buildTag "table" "class=\"histogram\"" $ buildOneTopicHelper (x, y, z) (x + y + z)

-- | Helper Method to generate the Histogram for one topic
buildOneTopicHelper :: (Int, Int, Int) -> Int -> String
buildOneTopicHelper _ 0 = []
buildOneTopicHelper (x, y, z) n
  | x < n && y < n && z < n = buildTag "tr" "" $ white ++ white ++ white ++ buildOneTopicHelper (x, y, z) (n -1)
  | x >= n && y >= n && z < n = buildTag "tr" "" $ black ++ black ++ white ++ buildOneTopicHelper (x, y, z) (n -1)
  | x < n && y < n && z >= n = buildTag "tr" "" $ white ++ white ++ black ++ buildOneTopicHelper (x, y, z) (n -1)
  | x < n && y >= n && z >= n = buildTag "tr" "" $ white ++ black ++ black ++ buildOneTopicHelper (x, y, z) (n -1)
  | x >= n && y >= n && z >= n = buildTag "tr" "" $ black ++ black ++ black ++ buildOneTopicHelper (x, y, z) (n -1)
  | x < n && y >= n && z < n = buildTag "tr" "" $ white ++ black ++ white ++ buildOneTopicHelper (x, y, z) (n -1)
  | x >= n && y < n && z < n = buildTag "tr" "" $ black ++ white ++ white ++ buildOneTopicHelper (x, y, z) (n -1)
  | x >= n && y < n && z >= n = buildTag "tr" "" $ black ++ white ++ black ++ buildOneTopicHelper (x, y, z) (n -1)

-- | Render black Square for Histogram
black :: String
black = buildTag "td" "bgcolor=\"grey\" style=\"height:20px;width:20px\"" ""

-- | Render white Square for Histogram
white :: String
white = buildTag "td" "bgcolor=\"white\" style=\"height:20px;width:20px\"" ""

-- | Builds Td Tag for Td Elements
buildTd :: [String] -> String
buildTd [] = []
buildTd (x : xs) = buildTag "td" "" x ++ buildTd xs

-- | Build Th Tags
buildTh :: [String] -> String
buildTh [] = " "
buildTh (x : xs) = buildTag "th" "" x ++ buildTh xs

-- | Build Tr Tags
buildRow :: [Html] -> String
buildRow [] = " "
buildRow (x : xs) = buildTag "tr" "" $ addTag x ++ buildRow xs

-- | Build a Table
buildTable :: [Html] -> String
buildTable [] = " "
buildTable (x : xs) = addTag x ++ buildTable xs

-- | Build li element
addListElement :: [Html] -> String
addListElement [] = " "
addListElement (x : xs) = buildTag "li" "" $ addTag x ++ addListElement xs

-- | Build a Form
addFormElement :: [String] -> String
addFormElement [] = []
addFormElement (x : xs) = buildTag "label" "for=\"fname\"" x ++ buildTag "input" ("type=\"text\" id=\"formular\" name=\"" ++ x ++ "\"") "" ++ "<br>" ++ addFormElement xs
