module Moviestore where

import Data.List (intercalate)

type Id         = Int
type Name       = String
type FSK        = Int
type Movie      = (Id, Name, FSK)
type Moviestore = ( [Movie] {- verfügbare Filme-}
                  , [Movie] {- ausgeliehene Filme -})

myMovieStore :: Moviestore
myMovieStore = ( [ (1, "Matrix", 16)
                 , (2, "Alpen - unsere Berge von oben", 0)
                 ]
               , [ (3, "The Breakfast Club", 12)
                 ]
               )

showMovie :: Movie -> String
showMovie (i, name, fsk) = name ++ " (Id: " ++ show i ++ ", FSK " ++ show fsk ++ ")"

showMovieList :: [Movie] -> String
showMovieList [] = ""
showMovieList (m:ms) = showMovie m ++ "\n" ++ showMovieList ms

showMoviestore :: Moviestore -> String
showMoviestore (available, rentable) =
  let availableCount = show $ length available
      rentCount      = show $ length rentable
  in    "Verfügbare Filme (" ++ availableCount ++ ")\n"
     ++ "===================" ++ replicate (length availableCount) '=' ++ "\n"
     ++ showMovieList available
     ++ "\nAusgeliehene Filme (" ++ rentCount ++ ")\n"
     ++ "=====================" ++ replicate (length rentCount) '=' ++ "\n"
     ++ showMovieList rentable

extract :: Id -> [Movie] -> (Maybe Movie, [Movie])
extract _ [] = (Nothing, [])
extract i (movie@(j,_,_):rest)
    | i == j    = (Just movie, rest)
    | otherwise = let (m,ms) = extract i rest
                  in (m, movie:ms)

return :: Id -> Moviestore -> Moviestore
return ident moviestore@(available, rentable) =
    let (maybeMovie, movies) = extract ident rentable
    in case maybeMovie of
           Just m -> (m:available, movies)
           _      -> moviestore

type Age = Int

rent :: Age -> Id -> Moviestore -> (Bool, Moviestore)
rent age ident moviestore@(available, rentable) =
    let (maybeMovie, movies) = extract ident available
    in case maybeMovie of
           Just movie@(_,_,fsk) | age >= fsk
                -> (True, (movies, movie:rentable))
           _    -> (False, moviestore)

-- Blatt 05

add :: (Name, FSK) -> Moviestore -> Moviestore
add (name,fsk) (available, rentable) =
  let findHighestIdInList :: [Movie] -> Id
      findHighestIdInList = maximum . map (\(i,_,_) -> i)
  in ( (1 + max (findHighestIdInList available) (findHighestIdInList rentable)
       , name, fsk) : available
     , rentable )

availableForAgeOf :: Age -> Moviestore -> [Movie]
availableForAgeOf age (available,_) =
    filter (\(_,_,fsk) -> fsk <= age) available

showNumberedMovieList :: [Movie] -> String
showNumberedMovieList movies =
    intercalate "\n" $
        zipWith (\ i movie -> show i ++ ". " ++ showMovie movie)
                ([1..] :: [Int])
                movies

showAvailableForAge :: Age -> Moviestore -> String
showAvailableForAge age = showNumberedMovieList . availableForAgeOf age
