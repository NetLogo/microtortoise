# Microtortoise

## What is it?

Ants, but small.

## How do you use it?

Get a [Galapagos](www.github.com/NetLogo/Galapagos) instance running on port 9000.  Galapagos commit ec944e6fd8 is confirmed to work.

### Scala

```sh
cd scala
sbt "run bench" # Generates a performance number
sbt "run json"  # Dumps out a JSON file into the root of this repo
```

### TypeScript

```sh
cd typescript
npm install
npm run build
```

### Viewing the model in action

From the root of the repository, launch an HTTP server, like this:

```sh
python3 -m http.server 8080
```

Then:

  * Navigate to http://localhost:8080 in your browser
  * Click "Setup"
  * Click "Go"

This will allow you to watch the TypeScript version in action.

You can click "Benchmark" and wait ~20 seconds, and you will eventually get a pop-up that tells you how many milliseconds it took to run the TypeScript version of the model.

You can click "Play Recording (Scala)" to watch a JSON file that was exported by Scala.  It is expected to be found at the URL `/microtortoise-data-scala.json`.  A similar thing can be done (presumably, for TypeScript) by getting another data file hosted at the URL `/microtortoise-data.json`.  Such a recording can be created with the "Make Recording" button.
