# cp-to-cpp-transpiler

### Compiling `.cp` files to `.cpp`

This is an MVP implementation for transpiling `.cp` files into `.cpp`. The `.cp` language syntax is Python-like while the generated output is optimized for competitive programming in C++.

---

### Features:
1. Minimalistic parser (Python-like syntax),
2. Transpiler generates `#include`, `iostream` headers for competitive programming,
3. Modular structure for extensions (standard-library optimizations).

### How to Run

1. **Using Stack:**
    ```bash
    stack build
    stack exec cp-to-cpp sample.cp output.cpp
    ```
2. **Sample Code (`cp`):**
    ```python
    print("Hello, World!")
    ```
3. **Generated Code (`cpp`):**
    ```cpp
    #include <iostream>
    using namespace std;

    // Transpiled line: print("Hello, World!")
    ```

### Extend Functionality
- Implement advanced parsing (`src/Parser.hs`)