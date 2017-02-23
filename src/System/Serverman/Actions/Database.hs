module System.Serverman.Actions.Database (DatabaseParams(..), dummy) where
  import System.Serverman.Utils
  import System.Serverman.Services

  import Control.Monad.Free

  data DatabaseParams = DatabaseParams { database        :: String
                                       , databaseService :: Service
                                       , dummyData       :: Bool
                                       , databaseUser    :: String
                                       , databasePass    :: String
                                       , databaseHost    :: String
                                       } deriving (Eq)

  dummy = ("serverman_users", ["first_name", "last_name", "email", "children", "birth_date", "gender"], [
                              ["MacKenzie","Wilcox","vel.sapien.imperdiet@bibendumsedest.com","4","1997-10-30T06:29:02-08:00","Male"],
                              ["Martha","Elliott","Phasellus@luctusetultrices.com","3","2000-03-04T00:53:32-08:00","Male"],
                              ["Declan","Nash","ut.quam@ultriciessemmagna.net","1","1975-08-02T00:27:02-07:00","Female"],
                              ["Kasimir","Fisher","sit.amet.consectetuer@sapien.net","2","2015-06-09T21:45:41-07:00","Male"],
                              ["Uma","Kelley","vulputate@maurisa.ca","2","1990-05-27T06:28:00-07:00","Female"],
                              ["Hayley","Owen","eu.eros@velnisl.org","4","2013-07-02T06:13:04-07:00","Male"],
                              ["Pamela","Hebert","vestibulum.lorem@molestietellus.net","1","1998-01-21T05:32:18-08:00","Female"],
                              ["Sydnee","Irwin","ultrices@consectetueradipiscing.edu","4","1984-01-15T22:55:10-08:00","Female"],
                              ["Brandon","Sharp","non@nunc.co.uk","3","2000-06-21T10:05:13-07:00","Female"],
                              ["Gray","Guerrero","ipsum@magnaUttincidunt.net","1","1975-12-02T06:59:56-08:00","Female"],
                              ["Nomlanga","Mercado","dolor.Quisque.tincidunt@Donec.edu","1","2015-07-04T01:21:44-07:00","Male"],
                              ["Luke","Frazier","Aenean.sed.pede@Etiamvestibulum.co.uk","1","2007-01-22T22:03:24-08:00","Male"],
                              ["Cynthia","Farmer","vel@eratEtiam.co.uk","3","1975-06-20T06:40:51-07:00","Female"],
                              ["Timothy","Hopper","magna.Praesent.interdum@Phasellusvitaemauris.org","1","1991-03-18T15:36:03-08:00","Male"],
                              ["Graiden","Walton","est.mauris@aultricies.edu","1","1997-12-06T10:35:10-08:00","Female"],
                              ["Abigail","Webster","elementum.dui@Duissitamet.com","4","1978-05-03T13:39:42-07:00","Female"],
                              ["Samuel","Dyer","parturient.montes@Etiamligula.org","3","2002-08-30T21:34:17-07:00","Female"],
                              ["May","Blackburn","montes.nascetur.ridiculus@Aliquameratvolutpat.org","1","2004-11-01T13:10:43-08:00","Female"],
                              ["Regina","Hicks","Sed.nulla.ante@atpretium.edu","2","2005-08-28T02:52:49-07:00","Female"],
                              ["Roth","Bright","lacus@feugiattellus.edu","4","2010-07-26T14:27:31-07:00","Male"],
                              ["Sylvester","Chapman","Sed.eu@sitametdiam.edu","4","1975-01-23T19:36:26-08:00","Male"],
                              ["Martin","Sharp","Nullam@Vivamusnibh.net","2","2016-10-18T23:48:20-07:00","Male"],
                              ["Mary","Schroeder","sem.egestas.blandit@nullaatsem.com","1","1993-03-16T17:41:10-08:00","Female"],
                              ["Blythe","Alston","amet.faucibus.ut@ornareFuscemollis.org","1","1980-09-22T04:58:53-07:00","Female"],
                              ["Nathan","Ramsey","in.molestie@Mauris.ca","4","2006-05-07T08:30:57-07:00","Female"],
                              ["Zelenia","Meadows","nunc@Aenean.com","3","1983-04-03T01:42:18-08:00","Female"],
                              ["Karyn","Booker","tempor@sagittissemperNam.ca","3","2006-10-13T02:29:44-07:00","Male"],
                              ["Hiram","Booth","semper@risusDonecegestas.ca","4","2001-10-30T19:53:13-08:00","Male"],
                              ["Robert","Mcclure","semper@nonduinec.org","1","2012-11-14T17:32:09-08:00","Female"],
                              ["Celeste","Callahan","convallis@NulladignissimMaecenas.edu","1","1984-08-22T22:56:35-07:00","Female"],
                              ["Magee","Olsen","ligula.consectetuer.rhoncus@fermentumvel.com","2","1978-04-09T15:12:05-08:00","Female"],
                              ["Dana","Mccullough","ut.sem.Nulla@eleifendnec.net","4","2000-08-23T07:54:53-07:00","Male"],
                              ["Yen","Blanchard","et@Morbi.org","3","1997-05-09T03:30:56-07:00","Male"],
                              ["Cora","Valdez","lorem.vitae.odio@vulputateullamcorpermagna.net","2","1998-10-24T16:06:46-07:00","Male"],
                              ["Amela","Blackburn","vulputate.dui@ultrices.co.uk","3","2006-03-08T07:42:27-08:00","Male"],
                              ["Dean","Blanchard","ac.tellus@nonummyipsumnon.co.uk","2","2014-12-21T14:38:37-08:00","Female"],
                              ["Alika","Shields","est.mauris@mollis.co.uk","3","1976-11-08T22:32:16-08:00","Female"],
                              ["Byron","Dudley","mattis@nequeNullam.org","1","1992-07-04T12:32:20-07:00","Female"],
                              ["Noelle","Young","et.malesuada.fames@aliquetmolestietellus.net","2","2009-04-05T03:05:01-07:00","Female"]])
