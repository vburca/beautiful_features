#
# File:   scrape_them.rb
# Date:   18 March 2013
# Author: Vlad Burca
#
# Description: 
#       Ruby script that scrapes the HTML of www.beautifulpeople.com
#       rating page and extracts information about the applicants.
# 
#       Purpose of the script is for the CPSC415-02 (Data Mining)
#       project, 'What features are most likely to get you into 
#       BeautifulPeople.com', by Vlad Burca and Zach Freedman, at
#       Trinity College, Hartford, CT
#

require 'rubygems'
require 'nokogiri'

# Filename variables
########################################################

HTML_NAME = 'female'

TEMP_NAME = 'saved_index'
TEMP_FILENAME = TEMP_NAME + '.txt'

FILE_NAME = 'data'
# add uniqueness to filename, in case of multiple scans
# FILE_UNIQUE_ID = Time.now.to_i
# FILENAME = FILE_NAME + '_' + FILE_UNIQUE_ID.to_s + '.txt'
FILENAME = FILE_NAME + '.txt'

########################################################

# RATINGS and CATEGORIES found on the rating page
########################################################

RATINGS = ['RVI1P', 'RVI2P', 'RVI3P', 'RVI4P']
CATEGORIES = ['profilehaircolor', 'profilejob', 
              'profileeyecolor', 'profilesmoking',
              'profilebodytype', 'profilecarowner',
              'profilezodiacsign', 'profilehomeowner',
              'profilerelationship'
             ]
COUNTRY_CATEGORY = 'rated_profilecountry'

########################################################


# Main program

data_file = File.open(FILENAME, 'a') rescue nil

if data_file.nil?
  puts "Error opening the file #{FILENAME}"
else
  # try to open last used index and read index
  temp_file = File.open(TEMP_FILENAME, 'r') rescue nil

  if temp_file.nil?
    # no such file yet
    puts "No current temporary file that contains last used index."
    puts "Creating a new one... #{TEMP_FILENAME}."
    # start from first ID
    index = 1
  else
    # there is a last used index file
    # set the index to the last used one + 1
    puts "Found temporary file with last used index."
    index = temp_file.first.to_i + 1
    temp_file.close
    puts "Starting from index= #{index}."
  end

  # try and open the HTML corresponding to that female ID
  html_filename = HTML_NAME + "_#{index}" + '.html'
  while page = Nokogiri::HTML(open(html_filename)) rescue nil do
    puts "Scraping through: \t #{html_filename}  ..."

    result = "#{index}" + ' '

    RATINGS.each do |rating|
      rating_extract = page.css("div[id=#{rating}]")[0]['style']

      rating_value = rating_extract.scan(/(\d+[.]\d+|\d+)/)[0][0]

      # round the value to 2 decimal points
      rating_value = rating_value.to_f.round(2).to_s

      result = result + rating_value + ' '
    end

    CATEGORIES.each do |category|
      category_extract = page.css("span[id=#{category}]")[0].text
      category_value = category_extract

      # deal with whitespace in the category value
      if category_value.include? ' '
        category_value = category_extract.split(' ')
        # capitalize second word in the value
        category_value[1] = category_value[1].capitalize
        # re-assemble the new category value
        category_value = category_value.join
      end

      # if the female did not provide any value for this category
      if category_value.empty?
        category_value = '*'
      end

      result = result + category_value + ' '
    end

    country_extract = page.css("td[id=#{COUNTRY_CATEGORY}]")[0].text
    country_value = country_extract

    # deal with whitespace in the category value
    if country_value.include? ' '
      country_value = country_extract.split(' ')
      # capitalize second word in the value
      country_value[1] = country_value[1].capitalize
      # re-assemble the new category value
      country_value = country_value.join
    end    

    result = result + country_value + ' '

    result = result + "\n"
    data_file.write(result)

    # advance to the next file
    index = index + 1
    html_filename = HTML_NAME + "_#{index}" + '.html'
  end

  if page.nil?
    puts "No more HTML profiles left to scrape through."
  end

  puts "Writing the last index= #{index - 1} to #{TEMP_FILENAME}."
  # write the last female index that was scraped
  temp_file = File.open(TEMP_FILENAME, 'w') rescue nil
  temp_file.write(index - 1)
  temp_file.close
end

data_file.close