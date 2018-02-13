system("wget -o nfl.json http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json")

system("cat nfl.json")

install.package('jsonlite')

install.package('curl')

library('jsonlite')

library('curl')

mydf<- fromJSON('nfl.json')

class(mydf&players)

head(mydf&players)


