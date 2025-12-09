# ollama-coder-bench

Local Ollama coder model benchmark for TypeScript.

## Usage
`npm install`
Copy the example.env to .env and fill in the values.
`cp example.env .env`

### Basic Usage
1. Prepare models (pulls from Ollama):
```bash
# Default (pulls models from models/coders.json)
npm run prepare-models

# Pull specific category of models
npm run prepare-models -- --models tiny    # (< 8GB RAM)
npm run prepare-models -- --models small   # (8-16GB RAM)
npm run prepare-models -- --models medium  # (16-32GB RAM)
npm run prepare-models -- --models large   # (32-64GB RAM)
npm run prepare-models -- --models huge    # (> 64GB RAM)

# Or limit to first 10 models in the default list:
npm run prepare-models -- 10
```

### Benchmarking Commands

#### `npm run bench`
The standard benchmark runner. It connects to an *existing* running Ollama instance (default: `localhost:11434`).

**Options:**
- `--models <name|file>`: Model list to use. Can be a preset name (`coders`, `tiny`, `small`, `medium`, `large`, `huge`) or a path to a JSON file (default: `coders`).
- `--scenarios <dir>`: Path to scenarios directory (default: `scenarios`).
- `--out <file>`: Output file for results (default: `results.json`).
- `--concurrency <n>`: Number of parallel scenarios to run (default: `1`).
- `--filter-model <pattern>`: Regex filter for model names (e.g. `ts-.*`).
- `--filter-scenario <pattern>`: Regex filter for scenario IDs.
- `--sequential-models`: Run models one by one to save VRAM, while still parallelizing scenarios (via `--concurrency`).
- `--limit <n>`: Limit the benchmark to the first `n` models in the list.
- `--main-gpu <n>`: Index of the main GPU to use (useful for multi-GPU setups).
- `--num-gpu <n>`: Number of layers to offload to GPU (set to `999` for max offload).
- `--num-ctx <n>`: Context window size (e.g., `4096`).
- `--num-thread <n>`: Number of CPU threads to use (if running on CPU).

**Example:**
```bash
# Run the 'tiny' models
npm run bench -- --models tiny --concurrency 5

# Run the default 'coders' list with sequential models
npm run bench -- 5 --sequential-models --concurrency 5 --main-gpu 0 --num-gpu 999
```

#### `npm run bench-max`
A helper script that **automates the Ollama server configuration** for maximum throughput.
1. Kills any existing Ollama instance.
2. Starts a dedicated Ollama server with:
   - `OLLAMA_NUM_PARALLEL=10` (Allows 10 concurrent requests per model).
   - `OLLAMA_MAX_LOADED_MODELS=1` (Forces unloading old models to free VRAM).
3. Runs the benchmark with optimized defaults.
4. Cleans up (kills Ollama) after finishing.

**Defaults:**
- `limit`: 10 models
- `concurrency`: 10 parallel scenarios
- `sequential-models`: true
- `main-gpu`: 0
- `num-gpu`: 999

**Overriding Defaults:**
You can pass overrides using `key=value` syntax to avoid npm parsing issues on Windows, or use standard flags if your environment supports it.

**Example: Targeting a specific secondary GPU (e.g. index 1)**
```bash
npm run bench-max -- main-gpu=1
```

**Example: Running more models with higher concurrency**
```bash
npm run bench-max -- limit=50 concurrency=10
```

## HARD → EASY: LLM Code Generation + Automated Testing Difficulty Ranking

Below is a **model-centric ranking** of programming languages from **hardest to test with an LLM** → **easiest**, based on real-world constraints such as:

* strictness of the language,
* complexity of toolchains,
* availability of deterministic test runners,
* surrounding ecosystem (build systems, compilers, dependencies),
* how brittle the language is to small errors,
* how well LLMs were trained on that language.

This is **not** a ranking of languages by difficulty for humans — it’s specifically about **LLM-driven automated testing**, where the model must **generate code**, **fix code**, and **pass automated builds/tests**.

### **1. C / C++ (most difficult)**

**Why it's hardest:**

* Extremely strict memory model.
* Small mistakes = segfaults, UB, non-deterministic failures.
* Requires complex build systems (CMake, Make, vcpkg, Conan).
* Headers, linking, ABI issues, platform differences.
* LLMs struggle with pointer semantics and correct memory layouts.
* Unit testing frameworks (GoogleTest, Catch2) add complexity.

### **2. Rust**

**Why:**

* Compiler is unforgiving (borrow checker).
* Minor mistakes prevent compilation altogether.
* LLMs often create borrow checker conflicts or lifetimes mismatches.
* Cargo makes setup easy, but correctness is very hard.
* Tests run easily, but *getting code to compile* is what kills models.

### **3. Haskell / OCaml (Hard Functional)**

**Why:**

* Strong, static, expressive type systems → LLMs produce mismatched types often.
* Functional purity constraints.
* Complex monadic patterns that models incorrectly chain.
* OCaml has syntax quirks (module system, optional semicolons) that confuse models.

### **4. Scala (especially Scala 3)**

**Why:**

* Very large, complex type system.
* Models confuse implicits / givens / typeclass constraints.
* sbt build complexity.
* Functional + OO hybrid that models mis-handle.

### **5. Java (with frameworks)**

**Why:**

* Verbosity → lots of surface area for errors.
* Spring Boot dependency injection is complex; LLMs misconfigure annotations.
* JUnit tests are easy though.
* Models sometimes hallucinate imports or classpaths.

### **6. C#**

**Why:**

* Similar to Java but simpler to assemble.
* Fewer ecosystem landmines.
* Models occasionally hallucinate .NET APIs or namespaces.

### **7. Go**

**Why:**

* Simple syntax, but strict interfaces and error handling patterns.
* LLMs sometimes misuse concurrency (goroutines, channels).
* Tests are straightforward, making it easier than Java/C#.

### **8. Dart**

**Why:**

*   Sound null safety and strict type system.
*   Tooling (`dart analyze`) is strict but helpful.
*   Async concepts (`Future`) sometimes trip up models.
*   Slightly less training data than TS/Python but high quality (Flutter).

### **9. TypeScript (moderately easy)**

**Why:**

* Type system is strict but forgiving enough.
* Ecosystem well understood by modern models.
* LLMs occasionally break build tooling or misuse advanced types (mapped types, discriminated unions).
* ESLint/Prettier constraints add surface area.

(*This is why a TypeScript benchmark is a great starting point.*)

### **10. Python**

**Why it’s easier:**

* Dynamic typing → fewer compilation blockers.
* Unit tests (pytest) are trivial.
* LLMs excel in Python due to training prevalence.
* Hard parts: imports and environment management, but still easier than TS/Java.

### **11. Ruby**

**Why:**

* Very permissive language.
* Test frameworks (RSpec/Minitest) easy.
* LLMs generate valid Ruby fairly reliably.
* Hard parts: Rails magic and metaprogramming.

### **12. PHP**

**Why:**

* Very permissive.
* LLMs generate syntactically correct PHP without much difficulty.
* Tests with PHPUnit are simple.

### **13. Bash / Shell scripts**

**Why:**

* Easy for LLMs to generate syntax but…
* Hard to test deterministically because shell code often depends on environment.
* Still overall easier than compiled languages.

### **14. HTML/CSS**

**Why it’s almost trivial:**

* No compilation step.
* No runtime errors.
* Validation is straightforward.

### **15. SQL (easiest for models to generate & test)**

**Why it’s easiest:**

* LLMs have extremely strong SQL generation performance.
* Tests are deterministic: run query → compare output.
* Syntax surface is small and patterns are predictable.

# Final Ordered List (Hardest → Easiest)

| Rank   | Language            | Difficulty Reason                                |
| ------ | ------------------- | ------------------------------------------------ |
| **1**  | **C / C++**         | Memory safety, linking, UB, toolchain complexity |
| **2**  | **Rust**            | Borrow checker, lifetimes, strict compiler       |
| **3**  | **Haskell**         | Complex types, functional purity, monads         |
| **4**  | **OCaml**           | Functional features, strict typing, module churn |
| **5**  | **Scala**           | Complex types, implicit/given confusion          |
| **6**  | **Java**            | Framework complexity, annotations                |
| **7**  | **C#**              | Namespace/API hallucinations                     |
| **8**  | **Go**              | Interface correctness, concurrency               |
| **9**  | **Dart**            | Sound null safety, async patterns                |
| **10** | **TypeScript**      | Types + linting + ecosystem surface area         |
| **11** | **Python**          | Dynamic, model-friendly                          |
| **12** | **Ruby**            | Easy but can hallucinate Rails magic             |
| **13** | **PHP**             | Straightforward, permissive                      |
| **14** | **Bash**            | Env-dependent but simple                         |
| **15** | **HTML/CSS**        | No runtime errors                                |
| **16** | **SQL**             | Highly deterministic, models excel               |

## Benchmark Methodology

This benchmark is designed to be **unforgiving**. Unlike standard evaluations that check if an LLM can solve a LeetCode problem, this framework tests if an LLM can act as a **Senior Software Engineer**.

### 1. The Core Loop
The execution order is now **Hardest -> Easiest** (C++ -> SQL). This ensures that the most brittle and complex scenarios are attempted first, providing early signals on model capability.

For every model and every scenario, the framework performs a rigorous lifecycle check:
1.  **Prompting**: The model is given a "Prompt File" (e.g., `prompt.md`) representing a ticket or requirements document. It must implement the solution in a specific file.
2.  **Generation**: The model generates the full source code.
3.  **Extraction**: The code block is parsed. If the model output is chatty or malformed, it counts as an instruction violation.
4.  **Compilation (Build)**:
    *   **TypeScript**: Runs `npm run build` (tsc) with `strict: true`.
    *   **Python**: Runs `python -m py_compile`.
    *   **Compiled (C++, Rust, Go, Java, C#, Scala, Haskell, OCaml)**: Runs full compiler toolchains (g++, cargo, go build, javac, dotnet, scalac, ghc, ocamlopt).
    *   **Scripting (Ruby, PHP, Bash)**: Runs syntax checkers (`ruby -c`, `php -l`, `bash -n`).
    *   **Other (HTML, SQL)**: Validates structure or schema correctness.
5.  **Linting**:
    *   **TypeScript**: `eslint`.
    *   **Python**: `pylint`.
    *   **Others**: Uses standard linters where available (e.g., `cargo clippy`, `go vet`).
6.  **Testing**:
    *   Runs robust test suites (Vitest, Pytest, XUnit, JUnit, Go Test, RSpec, etc.).

### 2. The Scoring System

Code generation is not binary (works/doesn't work). A model that generates compiling code is better than one that generates hallucinated syntax. A model that passes strict linting is better than one that produces "working" spaghetti code.

The final **Score** is an average of the per-language scores. Each language score is calculated as follows:

#### **A. Base Accuracy (0 - 100 points)**
$$ Base = (CompileRate \times 30) + (LintCleanRate \times 20) + (TestPassRate \times 50) $$

*   **Compile Rate (30%)**: Does the code build? (e.g., `tsc`, `javac`, `cargo build`).
*   **Lint Clean Rate (20%)**: Is the code idiomatic? (e.g., `eslint`, `clippy`, `pylint`).
*   **Test Pass Rate (50%)**: Do the unit/stress tests pass?

#### **B. Speed Bonus**
We reward low latency for valid solutions.
$$ Bonus = \frac{2000}{MedianLatencyMs + 100} $$
*   *Example*: 1 second latency ≈ +1.8 points. 20 seconds latency ≈ +0.1 points.

#### **C. Overall Ranking Score**
$$ OverallScore = \frac{\sum LanguageScores}{Count(Languages)} $$
This ensures that being great at Python but terrible at Rust will drag your score down, enforcing a holistic measure of coding capability across the difficulty spectrum.

### 3. Language Specifics & Nuances

Different languages pose different challenges to LLMs. This benchmark attempts to normalize difficulty by enforcing "Production Grade" standards for each.

#### **High Difficulty (C++, Rust, Haskell, OCaml)**
*   **Challenge**: Memory safety, Borrow Checker, Pure Functional paradigms.
*   **Requirement**: Zero compiler warnings, idiomatic resource management.
*   **Typical Failure**: Models struggle to satisfy the borrow checker (Rust) or handle complex monads (Haskell) without hallucinations.

#### **Medium Difficulty (Java, C#, Scala, Go, Dart)**
*   **Challenge**: Verbosity, Strict Type Systems, Project Structures.
*   **Requirement**: Correct imports, package declarations, and type safety.
*   **Typical Failure**: Models often hallucinate APIs or miss specific import paths (e.g., `java.util.concurrent.ConcurrentHashMap`).

#### **Easier Difficulty (TypeScript, Python, Ruby, PHP)**
*   **Challenge**: Logic errors, Concurrency, Dynamic Typing pitfalls.
*   **Requirement**: Passing strict linters and stress tests.
*   **Typical Failure**: Runtime errors during stress testing (race conditions).

#### **Specialized (Bash, HTML, SQL)**
*   **Challenge**: Syntax quirks (Bash), Semantic correctness (HTML), Data logic (SQL).
*   **Requirement**: Output must produce exact expected behavior/looks.

### 4. Scenario Types
We don't test "Reverse a Linked List". We test:
*   **System Design**: Implement an LRU Cache with TTL (in 10+ languages).
*   **Concurrency**: Implement an async Task Queue.
*   **Systems Programming**: Parse a binary packet stream.
*   **Data/Scripts**: Log rotation, SQL Analytics, Semantic HTML.

## Windows Benchmark Summary 

Last updated: 2025-12-08T13:36:31.838Z 

** System Environment **
- ** OS **: win32 10.0.26100 (x64) 
- ** CPU **: Intel(R) Core(TM) i7-8565U CPU @ 1.80GHz (8 cores) 
- ** Memory **: 15.90 GB
- ** GPU **: GPU 0: NVIDIA GeForce MX150 (UUID: GPU-cb76dbaf-b12c-0637-77bb-ec29ee391731)


| Model | Score | C++ | Rust | Hs | OCaml | Scala | Java | C# | Go | Dart | TS | Py | Ruby | PHP | Bash | HTML | SQL | Latency (ms) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| starcoder:1b | 20.6 | 9 | 2 | 22 | 23 | 24 | 9 | 9 | 2 | 11 | 22 | 23 | 24 | 23 | 21 | 53 | 54 | 819 |
| qwen2.5-coder:0.5b | 17.7 | 7 | 0 | 20 | 20 | 20 | 7 | 7 | 0 | 0 | 20 | 20 | 20 | 20 | 20 | 50 | 51 | 18901 |

## MacOS Benchmark Summary 

Last updated: 2025-12-08T01:11:06.240Z 

** System Environment **
- ** OS **: darwin 25.1.0 (x64) 
- ** CPU **: Intel(R) Core(TM) i9-9980HK CPU @ 2.40GHz (16 cores) 
- ** Memory **: 64.00 GB
- ** GPU **: Intel UHD Graphics 630, AMD Radeon Pro 5500M, AMD Radeon RX 6800 XT


| Model | Score | C++ | Rust | Hs | Scala | Java | C# | Go | Dart | TS | Py | Ruby | PHP | Bash | HTML | SQL | Latency (ms) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| starcoder2:3b | 29.2 | 30 | 30 | 20 | 20 | 7 | 20 | 0 | 17 | 20 | 50 | 53 | 20 | 50 | 50 | 50 | 307146 |
| qwen2.5-coder:1.5b | 24.9 | 20 | 0 | 20 | 20 | 20 | 20 | 20 | 20 | 13 | 30 | 50 | 20 | 50 | 20 | 50 | 0 |
| qwen2.5-coder:0.5b | 24.7 | 13 | 0 | 20 | 20 | 7 | 20 | 0 | 0 | 20 | 50 | 50 | 20 | 50 | 50 | 50 | 142147 |
| starcoder:1b | 23.0 | 23 | 10 | 20 | 21 | 7 | 20 | 0 | 10 | 13 | 30 | 20 | 20 | 50 | 50 | 50 | 47618 |
| deepseek-coder:1.3b | 22.0 | 7 | 0 | 20 | 20 | 7 | 20 | 20 | 7 | 20 | 20 | 20 | 20 | 50 | 50 | 50 | 163415 |
| opencoder:1.5b | 22.0 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 50 | 20 | 20 | 0 |
| yi-coder:1.5b | 22.0 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 50 | 0 |
| qwen2.5-coder:3b | 21.6 | 20 | 20 | 20 | 20 | 20 | 20 | 13 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 50 | 0 |
| codegemma:2b | 20.0 | 7 | 7 | 20 | 20 | 7 | 20 | 13 | 7 | 20 | 20 | 20 | 20 | 50 | 20 | 50 | 146126 |
| deepcoder:1.5b | 20.0 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 0 |
