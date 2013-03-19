Script: scrape_them.rb
**********************

Input
*****

Nothing really - just have the HTML files (in the format described below
- Constant Variables)
in the same directory as the script.

Output
******

The output data will be in the file:  data.txt
The name of the output file can be changed (described below - Constant Variables).

The format of an entry is the following (also, example):

ID  RedVotes OrangeVotes  LightGreenVotes GreenVotes  HairColor FieldOfWork EyeColor  Smoking BodyType  CarOwner  ZodiacSign  HomeOwner RelationshipStatus  Country
1 43.68 19.11 76.44 136.5 DarkBrown Student DarkBrown No Average Yes Taurus No Single United
2 19.11 16.38 65.52 171.99 * * * * * No Leo No * 

The Votes are rounded to 2 decimal points.

Constant Variables
******************

HTML_NAME = name of the HTML files of the profiles (followed by '_' and the index of the profile)
TEMP_NAME = name of the file that will save the last used index
FILE_NAME = name of the file that will save the data that is scraped from the HTMLs


RATINGS = the rating categories from the rating webpage
CATEGORIES = the categories from the rating webpage (provided by applicants)