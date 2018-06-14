# Chardoku

Can you spell six 3-letter words (three words horizontally, three words vertically) without repeating any letter?

Work in progress. Started as a hack night project at https://www.meetup.com/Elm-OC

## To run locally using `elm-live`

```
./bin/run.sh
```

## To deploy to S3

```
./bin/deploy.sh
```

## Todo

- Don't skip to next cell when deleting
  - Maybe forego text fields entirely, and keep all the state in Elm?
- Highlight misspelled words
- Highlight duplicate letters
- Remove words with duplicate letters from master word list
- Juicy victory screen
- Host somewhere (github pages?) and share
