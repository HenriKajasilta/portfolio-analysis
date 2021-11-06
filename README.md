# portfolio-analysis
R codes to keep database updated and some tools for portfolio analysis. Codes in this repository
work only, if the database is correctly set up (table and column names ect.) and if your data is
therefore in a correct form.

##

In order for these codes to work, set up a MySQL database (here it is called 'stock'), which
includes four tables: 'company', 'deposit', 'finance' and 'transactions'.

For example, the table 'company' has 5 columns (all type VARCHAR): Ticker, Country, Sector, Product and Type

The rest of the tables' inputs can be checked from the StockMySQL.R file, in which it should be
pretty intuitive to pair required column names to tables.

In the SQL server we should also have a view called 'summary'. SQL code for this view can be
founded from the .txt file which is in the portfolio folder as well. This view is updated when
it is called from the 'GetPorfolio.R' file, so make sure that this view is available in MySQL.

![MySQL_page](https://user-images.githubusercontent.com/92798719/140605182-bf5c7a8b-924a-4400-9764-a6ae79fb6bc5.png)


An image of how the MySQL database should look like with the required tables and a view.


##

App.R is the file, where it is possible to run the code and open the UI. At this point, make sure that you have installed all the required packages, but also saved the
necessary variables to the global environment (e.g. country.names and sector.names) in order for 'StockMySQL.R' to use them. If everything is correctly set up and the
app.R file is run, then this interface should pop up (keep in mind, that the table view for you might be empty!):

![UI_View](https://user-images.githubusercontent.com/92798719/140605694-3d445cfa-df2d-4310-b387-1550c26ca381.png)


From the tab view at the top, it is possible to change the viewed table. All the updates should now affect the database as well so keep an eye on that!
