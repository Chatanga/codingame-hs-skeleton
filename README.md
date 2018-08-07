codingame-hs-skeleton
=====================

Example project for the [codingame-hs package](https://github.com/Chatanga/codingame-hs/).

Usage
-----

1) Install it.

If you don’t have Git, you can also download the code by hand from GitHub.

```bash
$ git clone https://github.com/Chatanga/codingame-hs.git
$ git clone https://github.com/Chatanga/codingame-hs-skeleton.git
```

2) Provide your account informations.

Modify the `credentials.json` file as well as the first line of code in `Codingame.hs`
with your Codingame account informations..

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
```

If you get a "Nothing returned" or an "internal error" result, it is probably because
you didn’t have entered the corresponding challenge yet. To remedy this situation,
just enter it by visiting the Codingame site competition section.

If all went well, you should have a `LastBattles` folder created in your working directory
with a JSON file in it. That’s a game result named by its Game ID. If you bot follow the
"# convention" explaned below, you should be able to replay this game locally. Note that
it is a replay, a post-mortem in other words. You can change the bot code along the way
leading to potentially different output, but it won't change the replay itself. Your bot’s
output are always ignored in a replay and it’s only a mean to debug a past game.

```haskell
λ> doReplay 123456789
...
```

(The Game ID will surely be different)

Since the replay is local and use your bot as is, you can easily put breakpoint to understand
its behavior, inspect any variable, produce meaningful
(diagrams)[https://archives.haskell.org/projects.haskell.org/diagrams/] to clarify things up,
etc.

Once your bot is ready to enter the Arena, you can submit it.

```haskell
λ> doSubmit
```

If it is your first submit, just way one minute or two that enough games are played by the server
before retrieving the list of your last completed battles.

```haskell
λ> doList
```

You can download any game result you are interrested using its Game ID.

```haskell
λ> doRetrieve
```

The saved game result is now available for a local replay.

```haskell
λ> :quit
```

The "# convention"
------------------

The skeleton use the Haskell bootstrap code provided by Codingame for the "Code of Kutulu" contest
with some minor modification:

    - The `BotRunner` module has been imported.
    - The main has been changed into `runMain = runBot True bot`.
    - The man function has been changed into `bot readLine writeLine`.
    - The now unecessary `hSetBuffering stdout NoBuffering` has been removed.
    - Every direct access to `stdin` has been modified to use the `readLine` function provided.
    - Every direct access to `stdout` has been modified to use the `writeLine` function provided.

With these changes, the bot will now automatically echo on `stderr` its input with a `#` prefix
That’s all that matter here and it can obviously be done by other means.
The benefit of the changes listed above are that they allow you bot to be used without additional
modification by our replay function.

The reason why we need to echo the input back into `stderr` is simply that the game result omit
`stdin` (and only provided `stderr` for your agents).
