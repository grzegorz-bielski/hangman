# hangman

A simple hangman game.

It's fetching random word from `https://randomword.com/`, if not available it will get random word from local `data/dict.txt` file as a fallback.

## Usage

- Install [stack]("https://www.haskellstack.org/")
- (optional) Install intero (you might also want to install [haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero) for vs code)

```
stack build intero
```

- Install project

```
stack build hangman
```

- Run the application

```
stack exec hangman
```
