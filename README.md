# Arbitrage Detection Engine

An advanced, multi-threaded C++ engine designed to detect and simulate arbitrage opportunities in foreign exchange (Forex) markets using graph algorithms.

The project models currencies as vertices in a directed graph where edge weights represent exchange rates. It implements two distinct methodologies for finding profitable trade loops:
1. **DFS-Based Live Search:** Fetches real-time Forex data from the Frankfurter API and searches for cycles using Depth-First Search (DFS).
2. **Bellman-Ford-Moore Simulation:** Finds negative-weight cycles in a simulated Forex market using log-transformed rates.

---

## 💡 The Mathematics of Arbitrage

Arbitrage occurs when a sequence of currency exchanges yields a net profit after transaction fees. Formally, we seek a cycle of currencies $C_0 \to C_1 \to \dots \to C_k \to C_0$ such that the product of exchange rates $R_{ij}$ exceeds $1$:

$$R_{01} \times R_{12} \times \dots \times R_{k0} > 1$$

To transform this multiplicative path problem into an additive shortest-path problem, we apply a negative logarithm to the exchange rates:

$$w_{ij} = -\ln(R_{ij})$$

Applying this transformation, the product inequality becomes:

$$-\ln(R_{01}) - \ln(R_{12}) - \dots - \ln(R_{k0}) < 0$$

$$\sum w_{ij} < 0$$

Thus, finding a profitable arbitrage opportunity is mathematically equivalent to detecting a **negative-weight cycle** in the graph.

---

## 🛠️ Repository Layout

The repository is structured into two clean directories:

```
Arbitrage_Detection/
├── README.md                   # Project developer guide
├── Algorithms/                 # Folder containing search methods
│   ├── dfs.hpp                 # DFS cycle search declarations
│   ├── dfs.cpp                 # DFS cycle search implementations
│   ├── bellman_ford.hpp        # Bellman-Ford path relaxation declarations
│   └── bellman_ford.cpp        # Bellman-Ford path relaxation implementations
└── Core/                       # Folder containing core utilities and entrypoints
    ├── main_dfs.cpp            # DFS live rates execution loop
    ├── main_bellman_ford.cpp   # Bellman-Ford simulation execution loop
    ├── ThreadPool.hpp          # Multi-threaded job scheduling pool
    ├── ArbitrageEngine.hpp     # Trading account declarations
    ├── ArbitrageEngine.cpp     # Trading account implementation
    ├── APIClient.hpp           # Frankfurter API HTTP client declarations
    ├── APIClient.cpp           # Frankfurter API HTTP client implementation
    ├── GraphUtils.hpp          # Graph creation & normalizer declarations
    ├── GraphUtils.cpp          # Graph creation & normalizer implementation
    ├── Config.hpp              # Currency configuration and SSL cert paths
    ├── json.hpp                # Redirect wrapper for json library
    ├── nlohmann/               # Raw nlohmann json header directory
    ├── curl/                   # Raw curl header directory (flattened)
    └── lib/                    # Raw static libraries directory (contains libcurl.a, etc.)
```

---

## 🚀 Compilation & Running

Open your PowerShell terminal in the repository root directory and run the compilation commands:

### 1. Simulated Bellman-Ford (`main_bellman_ford`)
Runs a simulator injecting a high-probability arbitrage cycle (INR $\to$ USD $\to$ JPY $\to$ AUD $\to$ INR) to demonstrate negative-cycle detection.

```powershell
# Compile the executable
g++ -std=c++17 -o Core/main_bellman_ford.exe Core/main_bellman_ford.cpp Core/GraphUtils.cpp Algorithms/bellman_ford.cpp Core/ArbitrageEngine.cpp -ICore -IAlgorithms

# Run the executable
.\Core\main_bellman_ford.exe
```

### 2. Live Market DFS (`main_dfs`)
Fetches live exchange rates relative to INR using Frankfurter API, simulates exchange transactions on the graph with randomized spreads, and logs profitable cycles.

```powershell
# Compile the executable (links local libcurl and Windows socket APIs)
g++ -std=c++17 -o Core/main_dfs.exe Core/main_dfs.cpp Core/GraphUtils.cpp Algorithms/dfs.cpp Core/ArbitrageEngine.cpp Core/APIClient.cpp -ICore -IAlgorithms -LCore/lib -lcurl -lws2_32 -lcrypt32 -lwldap32 -lnormaliz -lbcrypt

# Run the executable
.\Core\main_dfs.exe
```

> [!TIP]
> If your system requires custom SSL certificate bundles for curl, update the `Config::cacert` path in `Core/Config.hpp` to point to your `cacert.pem` file.
