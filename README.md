codingame-hs-skeleton
=====================

Example project for the [codingame-hs package](https://github.com/Chatanga/codingame-hs/).

Usage
-----

1) Install it.

If you don’t have Git, you can also download the code by hand from GitHub using the same
links.

```bash
$ git clone https://github.com/Chatanga/codingame-hs.git
$ git clone https://github.com/Chatanga/codingame-hs-skeleton.git
```

2) Provide your account informations.

Modify the `credentials.json` file as well as the first lines of code in `Codingame.hs`
with your Codingame account informations. As you will see, the "Code of Kutulu" challenge
has been used as an example for this skeleton, but it will work the same way with any
other.

3) Build the project

You obviously already have [Stack](https://docs.haskellstack.org/en/stable/README/)
installed on your machine.

```bash
$ cd codingame-hs-skeleton
$ stack setup
$ stack build
```

4) Launch GHCI


```bash
$ stack ghci
```

Let start by playing a private game into the Codingame IDE.

```haskell
λ> doPlay
Ranks: [1,0,3,2]
Game options: "seed=72353923\n"
Saved game result: 329737891
```

If you get a "Nothing returned" or an "internal error" result, it is probably because
you didn’t have entered the corresponding challenge yet. To remedy this situation,
just enter it by visiting the Codingame site competition section.

If all went well, you should have a `LastBattles` folder created in your working directory
with a JSON file in it. That’s a game result named by its Game ID. If you bot follow the
"# convention" explained below, you should be able to replay this game locally. Note that
it is a replay, a post-mortem in other words. You can change the bot code along the way
leading to potentially different output, but it won't change the replay itself. Your bot’s
output is always ignored in a replay and it’s only a mean to debug a past game.

```haskell
λ> doReplay 329737891
Two bots enter; one bot leaves!
WAIT
WAIT
WAIT
...
WAIT
Terminated
```

(The Game ID will obviously be different for you.)

Since the replay is local and use your bot as is, you can easily put breakpoints to understand
its behavior, inspect any variable, produce meaningful
[diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/) to clarify things up,
etc.

Once your bot is ready to enter the Arena, you can submit it.

```haskell
λ> doSubmit
Agent ID: 9534321
```

If it is your first submit, just way one minute or two that enough games are played by the server
before retrieving the list of your last completed battles.

```haskell
λ> doList
Completed last battles:
329738649 - 0: DeCe07 (1986268), 1: Programming_Doge (1963033), 2: Galrauch (1957957), 3: Aries (2008136)
329738648 - 0: Akhaten (1957475), 1: vampiro (1957611), 2: avdg (1971085), 3: Aries (2008136)
329738647 - 0: Mitrandir (1962812), 1: pwlrfl (1960146), 2: ereiam (1957444), 3: Aries (2008136)
329738646 - 0: DirtyHarry (1957503), 1: Testeris (1957423), 2: Alpacah (1957534), 3: Aries (2008136)
329738645 - 0: Samsa (1957525), 1: cdurbin (1957499), 1: JohnnyYuge (1957401), 3: Aries (2008136)
329738644 - 0: DorianWilde (1957553), 1: BeberLeNewbie (1957519), 2: FrancoisMeme (1963607), 3: Aries (2008136)
329738643 - 0: Mugu-Mugu (1993182), 1: Snowbart (1957510), 2: Bob (1957529), 3: Aries (2008136)
329738642 - 0: SiR_Jonny (1991026), 1: Ben-oit (1962936), 2: nevgen (1957488), 3: Aries (2008136)
329738641 - 0: Risen (1957522), 1: SF (1957407), 2: kiwijam (1970416), 3: Aries (2008136)
329738640 - 0: miklla (1957437), 1: RiSuS (1957539), 2: MSmits (1957549), 3: Aries (2008136)
```

You can download any game result you are interested in by using its Game ID.

```haskell
λ> doRetrieve 329738643
Ranks: [2,0,1,3]
Game options: "seed=33280016\n"
Saved game result: 329738643
```

The saved game result is now available for a local replay.

```haskell
λ> :quit
```

The "# convention"
------------------

The skeleton uses the Haskell bootstrap code provided by Codingame for the "Code of Kutulu" contest
with some minor modifications:

- The `BotRunner` module has been imported.
- A `runMain = runBot True bot` function (renamed as `main` by the packager) has been added.
- The original `main` function has been changed into a `bot :: Bot` function.
- The now unecessary `hSetBuffering stdout NoBuffering` has been removed.
- Every direct access to `stdin` has been modified to use the provided `readLine` function.
- Every direct access to `stdout` has been modified to use the provided `writeLine` function.

With these changes, the bot will automatically echo on `stderr` its input with a `#` prefix.
That’s all that matter here and it can obviously be done by other means. The benefit of the changes
listed above are that they allow your bot to be used without additional modifications by the
`replay` function in the `Codingame` module.

The reason why we need to echo the input back into `stderr` is simply that the game result omits
`stdin` (and only provided `stderr` for your agents).
