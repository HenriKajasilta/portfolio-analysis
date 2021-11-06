# portfolio-analysis
R codes to keep database updated and some tools for portfolio analysis. Codes in this repository
works only, if database is correctly set up (table and column names ect.) and if your data is
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

![MySQL_page](https://user-images.githubusercontent.com/92798719/140605182-bf5c7a8b-924a-4400-9764-a6ae79fb6bc5.png)
Picture how the MySQL database should look like with the required tables and a view.


##

App.R is the file, where it is possible to run the code and open the UI. At this point, take care that you have installed all the required packages, but also saved the
necessary variables to global environment (e.g. country.names and sector.names) in order to 'StockMySQL.R' use them. If everything is correctly set up and app.R file is run
then this interface should pop up (keep in mind, that the table view for you might be empty!):

![UI_View](https://user-images.githubusercontent.com/92798719/140605694-3d445cfa-df2d-4310-b387-1550c26ca381.png)


From the tab view at the top, it is possible to change the viewed table. All the updates should now affect to the database as well so keep an eye on that!
