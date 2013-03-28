#
# File:     scrape_them.rb
# Date:     18 March 2013
# Update:   27 Match 2013 
# Author:   Vlad Burca
#
# Description: 
#       Ruby script that scrapes the HTML of www.beautifulpeople.com
#       rating page and extracts information about the applicants.
# 
#       Purpose of the script is for the CPSC415-02 (Data Mining)
#       project, 'What features are most likely to get you into 
#       BeautifulPeople.com', by Vlad Burca and Zach Freedman, at
#       Trinity College, Hartford, CT8
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

RATINGS = { :rating_1p => 'RVI1P',
            :rating_2p => 'RVI2P',
            :rating_3p => 'RVI3P',
            :rating_4p => 'RVI4P',
          }

CATEGORIES = { :hair_color => 'profilehaircolor',
               :job => 'profilejob',
               :eye_color => 'profileeyecolor',
               :smoking => 'profilesmoking',
               :body_type => 'profilebodytype',
               :car_owner => 'profilecarowner',
               :zodiac_sign => 'profilezodiacsign',
               :home_owner => 'profilehomeowner',
               :relationship => 'profilerelationship'
             }

AGE_CATEGORY = 'rated_profileage'  # goes in :age => ''
COUNTRY_CATEGORY = 'rated_profilecountry' # goes in :country => ''

########################################################

ENTRIES = []


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
    start_index = 1
  else
    # there is a last used index file
    # set the index to the last used one + 1
    puts "Found temporary file with last used index."
    start_index = temp_file.first.to_i + 1
    temp_file.close
    puts "Starting from index= #{start_index}."
  end

  index = start_index

  # try and open the HTML corresponding to that female ID
  html_filename = HTML_NAME + "_#{index}" + '.html'

  # to take in account the first entry, which will always be incomplete
  first = true

  while page = Nokogiri::HTML(open(html_filename)) rescue nil do
    puts "Scraping through: \t #{html_filename}  ..."

    entry = { :id => index, 
              :rating_1p => '',
              :rating_2p => '',
              :rating_3p => '',
              :rating_4p => '',
              :age => '',
              :hair_color => '',
              :job => '',
              :eye_color => '',
              :smoking => '',
              :body_type => '',
              :car_owner => '',
              :zodiac_sign => '',
              :home_owner => '',
              :relationship => '',
              :country => '',
              :complete_entry => false
            }

    # add the skeleton for the new entry
    ENTRIES[index] = entry

    ## result = "#{index}" + ' '
    RATINGS.to_a.each do |rating_key, rating|
      rating_extract = page.css("div[id=#{rating}]")[0]['style']

      # check if it is the first profile we check
      if rating_extract && first == false
        rating_value = rating_extract.scan(/(\d+[.]\d+|\d+)/)[0][0]

        # round the value to 2 decimal points
        rating_value = rating_value.to_f.round(2).to_s

        # we have to add the rating to the previous entry in ENTRIES
        # (index - 1) = previous index in ENTRIES = id of the entry that we want
        ENTRIES[index-1][rating_key] = rating_value

        age_extract = page.css("td[id=#{AGE_CATEGORY}]")[0].text
        age_value = age_extract

        # just add the age to the current entry
        ENTRIES[index-1][:age] = age_value

        ## result = result + rating_value + ' '
      end
    end

    # since we added the ratings for the previous profile, we know that this is done!
    if index > start_index 
      ENTRIES[index-1][:complete_entry] = true
    end

    CATEGORIES.to_a.each do |category_key, category|
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

      # just add the category to the current entry
      ENTRIES[index][category_key] = category_value

      ## result = result + category_value + ' '
    end

    country_extract = page.css("td[id=#{COUNTRY_CATEGORY}]")[0].text
    country_value = country_extract

    age_extract = page.css("td[id=#{AGE_CATEGORY}]")[0].text
    age_value = age_extract

    # deal with whitespace in the category value
    if country_value.include? ' '
      country_value = country_extract.split(' ')
      # capitalize second word in the value
      country_value[1] = country_value[1].capitalize
      # re-assemble the new category value
      country_value = country_value.join
    end    

    # just add the country to the current entry
    ENTRIES[index][:country] = country_value

    ## result = result + country_value + ' '

    ## result = result + "\n"

    # Prepare the entry string for output in the data file
    if index > start_index
      # double check the entry before
      if ENTRIES[index-1][:complete_entry] == true
        result = "#{ENTRIES[index-1][:id]}" + ' '
        ENTRIES[index-1].to_a.each do |key, value|
          if key != :id and key != :complete_entry
            result = result + value + ' '
          end
        end
        #puts result
        result = result + "\n"
        data_file.write(result)
      end
    end

    # advance to the next file
    index = index + 1
    html_filename = HTML_NAME + "_#{index}" + '.html'

    first = false
  end

  if page.nil?
    puts "No more HTML profiles left to scrape through."
  end

  puts "Writing the last index= #{index - 1 - 1} to #{TEMP_FILENAME}."
  # write the last female index that was scraped
  temp_file = File.open(TEMP_FILENAME, 'w') rescue nil
  # index - 1 is the incomplete one, while index - 1 - 1 is actually the last WRITTEN index
  temp_file.write(index - 1 - 1)
  temp_file.close
end

data_file.close