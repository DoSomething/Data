xcode-select --install
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew doctor
brew install mysql
brew services start mysql
nano .my.cnf

#################
#[client]
#host=quasar-slave-new.c9ajz690mens.us-east-1.rds.amazonaws.com
#user=shasan
#password=SECRET
#################

mysql < Data/AdHoc/Scripts/test_query.sql > Desktop/outfile.csv