# portfolio-analysis
R codes to keep database updated and some analysis tools. Codes in this repository
works only, if database is correctly set up (table names ect.) and if your data is
therefore in a correct form.

##

In order these codes to work, set up a MySQL database (here it is called 'stock'), which
includes four tables: 'company', 'deposit', 'finance' and 'transactions'.

For example table 'company' has 5 columns (all type VARCHAR): Ticker, Country, Sector, Product and Type

Rest of the tables' inputs can be checked from StockMySQL.R file, in which it should be
pretty intuitive to pair column names to tables.

In the SQL server we should also have a view called 'summary'. SQL code for this view can be
founded from the .txt file which is in portfolio folder as well. This view is updated when
it is called from 'GetPorfolio.R' file, so take care that this view is available in MySQL.

