# ollama-coder-bench

Local Ollama coder model benchmark for TypeScript.

```bash
npm install
npm run prepare-models
npm run bench
npm run summarize
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

---

### **2. Rust**

**Why:**

* Compiler is unforgiving (borrow checker).
* Minor mistakes prevent compilation altogether.
* LLMs often create borrow checker conflicts or lifetimes mismatches.
* Cargo makes setup easy, but correctness is very hard.
* Tests run easily, but *getting code to compile* is what kills models.

---

### **3. Haskell / OCaml / Other ML-family languages**

**Why:**

* Strong, static, expressive type systems → LLMs produce mismatched types often.
* Functional purity constraints.
* Complex monadic patterns that models incorrectly chain.
* Cabal/Stack ecosystem for Haskell is nontrivial.

---

### **4. Scala (especially Scala 3)**

**Why:**

* Very large, complex type system.
* Models confuse implicits / givens / typeclass constraints.
* sbt build complexity.
* Functional + OO hybrid that models mis-handle.

---

### **5. Java (with frameworks)**

**Why:**

* Verbosity → lots of surface area for errors.
* Spring Boot dependency injection is complex; LLMs misconfigure annotations.
* JUnit tests are easy though.
* Models sometimes hallucinate imports or classpaths.

---

### **6. C#**

**Why:**

* Similar to Java but simpler to assemble.
* Fewer ecosystem landmines.
* Models occasionally hallucinate .NET APIs or namespaces.

---

### **7. Go**

**Why:**

* Simple syntax, but strict interfaces and error handling patterns.
* LLMs sometimes misuse concurrency (goroutines, channels).
* Tests are straightforward, making it easier than Java/C#.

---

### **8. TypeScript (moderately easy)**

**Why:**

* Type system is strict but forgiving enough.
* Ecosystem well understood by modern models.
* LLMs occasionally break build tooling or misuse advanced types (mapped types, discriminated unions).
* ESLint/Prettier constraints add surface area.

(*This is why a TypeScript benchmark is a great starting point.*)

---

### **9. Python**

**Why it’s easier:**

* Dynamic typing → fewer compilation blockers.
* Unit tests (pytest) are trivial.
* LLMs excel in Python due to training prevalence.
* Hard parts: imports and environment management, but still easier than TS/Java.

---

### **10. Ruby**

**Why:**

* Very permissive language.
* Test frameworks (RSpec/Minitest) easy.
* LLMs generate valid Ruby fairly reliably.
* Hard parts: Rails magic and metaprogramming.

---

### **11. PHP**

**Why:**

* Very permissive.
* LLMs generate syntactically correct PHP without much difficulty.
* Tests with PHPUnit are simple.

---

### **12. Bash / Shell scripts**

**Why:**

* Easy for LLMs to generate syntax but…
* Hard to test deterministically because shell code often depends on environment.
* Still overall easier than compiled languages.

---

### **13. HTML/CSS**

**Why it’s almost trivial:**

* No compilation step.
* No runtime errors.
* Validation is straightforward.

---

### **14. SQL (easiest for models to generate & test)**

**Why it’s easiest:**

* LLMs have extremely strong SQL generation performance.
* Tests are deterministic: run query → compare output.
* Syntax surface is small and patterns are predictable.

---

# Final Ordered List (Hardest → Easiest)

| Rank   | Language            | Difficulty Reason                                |
| ------ | ------------------- | ------------------------------------------------ |
| **1**  | **C / C++**         | Memory safety, linking, UB, toolchain complexity |
| **2**  | **Rust**            | Borrow checker, lifetimes, strict compiler       |
| **3**  | **Haskell / OCaml** | Complex types, functional purity                 |
| **4**  | **Scala**           | Complex types, implicit/given confusion          |
| **5**  | **Java**            | Framework complexity, annotations                |
| **6**  | **C#**              | Namespace/API hallucinations                     |
| **7**  | **Go**              | Interface correctness, concurrency               |
| **8**  | **TypeScript**      | Types + linting + ecosystem surface area         |
| **9**  | **Python**          | Dynamic, model-friendly                          |
| **10** | **Ruby**            | Easy but can hallucinate Rails magic             |
| **11** | **PHP**             | Straightforward, permissive                      |
| **12** | **Bash**            | Env-dependent but simple                         |
| **13** | **HTML/CSS**        | No runtime errors                                |
| **14** | **SQL**             | Highly deterministic, models excel               |

## Benchmark Summary

Last updated: 2025-12-06T03:50:00.245Z

| Model | Score | C++ | Rust | Hs | Scala | Java | C# | Go | TS | Py | Ruby | PHP | Bash | HTML | SQL | Latency (ms) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| qwen2.5-coder:0.5b | 35.1 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 20.1 | 50.1 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 26745 |
