# Data structures of the proposer and the end determination functions

This document specifies the **contract** that the two user supplied functions of
an `swfcalib` *job* must respect:

- `make_next_proposals` — the **proposer**: what to try next.
- `get_result` — the **end determination** function (`determ_end`): is this job
  finished, and if so with what values.

It describes what they receive, what they must return in each situation, and
shows concrete examples. The built-in factories (`make_shrink_proposer()`,
`make_proposer_se_range()`, `determ_poly_end()`, `determ_end_thresh()`) are used
as worked illustrations.

The running example is the one from the
[vignette](vignettes/swfcalib.Rmd): an HIV model with 9 calibrated parameters
and 9 outcomes, `n_sims = 400`.

---

## 1. Where the two functions are called from

Both functions are stored in the *job* and are invoked from
`calibration_step1()` (see `R/calibration_steps.R`), **in this order**, once per
iteration:

```
calibration_step1()
  |
  |-- process_sim_results()        # merge proposals + sim outputs -> results.rds
  |-- results <- load_results()    # all iterations of the CURRENT wave
  |
  |-- update_calibration_state()
  |     |-- get_jobs_results()     # (1) job$get_result(calib_object, job, results)
  |     |                          #     -> NULL or 1-row tibble, FOR EVERY JOB
  |     |-- update_done_status()   #     wave done if no job returned NULL
  |     |-- update_default_proposal()  # write returned values into default_proposal
  |     |-- update_wave_iteration()    # iteration + 1, or wave + 1
  |
  |-- make_proposals()
        |-- job$make_next_proposals(calib_object, job, results)   # (2)
        |     ... only for jobs NOT done, and only when iteration > 1
        |-- merge_proposals() / fill_proposals() / adjust_proposal_number()
```

Two consequences that drive everything below:

1. **`get_result` always runs before `make_next_proposals`.** A proposer may
   therefore rely on a *sideload* written by the end determination function of
   the same iteration (this is exactly how `make_shrink_proposer()` +
   `determ_poly_end()` work together).
2. **`get_result` is called for *every* job of the wave, including jobs that
   already converged**, while `make_next_proposals` is called **only for jobs
   that are not done** (`get_current_jobs(calib_object, not_done_only = TRUE)`).
   See §4.5 — a converged job must keep returning its result.

### Iteration numbering

| moment | `state$iteration` | `results` contains |
|-|-|-|
| calibration start | `0` | nothing (no `results.rds` yet) |
| 1st call to step 1 | `0 -> 1` | empty tibble; `get_result` is **not** called, `initial_proposals` is used |
| 2nd call to step 1 | `1 -> 2` | iteration 1 (`n_sims` rows) |
| N-th call to step 1 | `N-1 -> N` | iterations `1 .. N-1` |

So, at a given call of step 1:

- `get_result` sees results **up to and including** the iteration that just ran.
- `make_next_proposals` sees the **same** `results` object, and its output is
  labelled with the *next* iteration number.

Every new wave restarts this counter at `1` (`increment_wave()`): `results` is
empty again (it is read from the new wave's directory), the sideloads are
deleted, and `initial_proposals` is used once more. Neither function can see
anything from a previous wave except through `default_proposal`.

---

## 2. The inputs (identical for both functions)

Both functions have the signature:

```r
function(calib_object, job, results)
```

### 2.1 `calib_object`

The full calibration object (`R/calib_object.R`). Job functions should only
touch it through accessors; in practice they use it for two things:

```r
swfcalib::load_sideload(calib_object, job)          # read data saved earlier
swfcalib::save_sideload(calib_object, job, x)       # store data for later
swfcalib::get_default_proposal(calib_object)        # current best values, all params
```

Useful fields, for reference:

```r
calib_object$config$n_sims            # 400   - simulations per iteration
calib_object$config$max_iteration     # 100   - fail-safe
calib_object$config$default_proposal  # 1-row tibble, ALL calibrated parameters
calib_object$state$wave               # 1
calib_object$state$iteration          # 3
calib_object$state$done               # FALSE - whole wave done?
calib_object$state$jobs_done          # c(TRUE, FALSE, FALSE, FALSE)
```

The **sideload** is a per-job, per-calibration scratch file
(`<root_directory>/sideloads/wave1-job1.rds`). It holds anything (a list is
customary). It is **deleted when the wave changes** (`clear_sideloads()`), so a
sideload never survives from one wave to the next. `load_sideload()` returns
`NULL` when nothing was ever saved — which is the normal state at iteration 1.

### 2.2 `job`

The job definition, plus an `id` injected by `initialize_job_ids()`:

```r
job <- list(
  id          = "wave1-job4",                       # "wave<i>-job<j>"
  targets     = c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W"),
  targets_val = c(0.829, 0.898, 0.881),
  params      = c("tx.init.rate_1", "tx.init.rate_2", "tx.init.rate_3"),
  initial_proposals    = <tibble, n_sims rows, one column per param>,
  make_next_proposals  = <function>,
  get_result           = <function>
)
```

`targets[i]` is paired with `targets_val[i]`. Nothing in the framework pairs
`targets[i]` with `params[i]` — that pairing is a convention of the built-in
functions (see §5.3 and §5.4), not of `swfcalib` itself.

### 2.3 `results`

A tibble built by `merge_sim_results()` — a `right_join` of the proposals with
the simulation outputs — accumulated with `bind_rows` over **all iterations of
the current wave**. It is reset to an empty tibble at each new wave.

Columns, in order:

| block | columns | source |
|-|-|-|
| parameters | **every** parameter of `default_proposal` — not only `job$params` | proposals |
| bookkeeping | `.proposal_index`, `.wave`, `.iteration` | proposals |
| outcomes | **every** column returned by the simulator — not only `job$targets` | model |

Rows: `n_sims` per iteration (more if the simulator returns several rows per
proposal — the join keeps them all).

Example, for wave 1 at the moment `get_result` is called on iteration 2
(`n_sims = 400`, so 800 rows):

```
# A tibble: 800 x 21
   hiv.test.rate_1 hiv.test.rate_2 ... tx.init.rate_3 hiv.trans.scale_1 ...
             <dbl>           <dbl>              <dbl>             <dbl>
 1        0.002             0.00351           0.412               2.47
 2        0.00201           0.00587           0.187               2.47
 ...
   .proposal_index .wave .iteration cc.dx.B cc.dx.H ... i.prev.dx.W
             <int> <dbl>      <dbl>   <dbl>   <dbl>         <dbl>
 1               1     1          1   0.612   0.799         0.081
 2               2     1          1   0.615   0.902         0.079
 ...
```

Note that `hiv.trans.scale_1` is constant here: it is a wave-2 parameter, so
during wave 1 it is filled from `default_proposal`. Both functions must
therefore **subset explicitly** with `job$params` / `job$targets` and never
assume `results` only holds their own columns.

`results` can contain `NA` outcomes (a failed or degenerate simulation). Both
functions are responsible for dropping them, e.g.:

```r
ok <- vctrs::vec_detect_complete(results[, c(job$params, job$targets)])
results <- results[ok, ]
```

---

## 3. The proposer — `make_next_proposals`

### 3.1 Contract

```r
make_next_proposals <- function(calib_object, job, results) {
  # ...
  dplyr::as_tibble(<data>)
}
```

**It must return a `tibble` / `data.frame` with:**

| rule | detail |
|-|-|
| columns | exactly one column per name in `job$params`, named identically |
| no extra columns | anything else flows into the proposals and is passed to the simulator |
| rows | one row per parameter set to test; **should be `config$n_sims`** |
| types | the types expected by the simulator (usually `numeric`) |
| no `NA` | every row is going to be simulated |
| no bookkeeping | `.proposal_index`, `.wave`, `.iteration` are added by `swfcalib` |

Returning `NULL`, a vector, or a list is **not** valid — `merge_proposals()`
calls `nrow()` on the output.

### 3.2 What happens to the returned tibble

`make_proposals()` post-processes every job's output (`R/proposal.R`):

1. `merge_proposals()` — `bind_cols` all jobs of the wave. Jobs returning fewer
   rows than the longest one are padded by **resampling their own rows with
   replacement**.
2. `fill_proposals()` — columns of `default_proposal` absent from the proposals
   (parameters of other waves, or of jobs already converged) are appended with
   their current best value.
3. `adjust_proposal_number()` — the table is resampled down to, or padded up to,
   exactly `config$n_sims` rows.
4. `.proposal_index` (`1:n_sims`), `.wave` and `.iteration` are appended.

The practical rule: **return exactly `n_sims` rows**. Fewer means some of your
proposals are duplicated at random; more means some are silently dropped.

### 3.3 Example — single parameter, converging

Wave 1 / job 1: `params = "hiv.test.rate_1"`, `n_sims = 400`.

*Iteration 1* — the proposer is **not called**; `job$initial_proposals` is used:

```r
dplyr::tibble(hiv.test.rate_1 = seq(0.002, 0.006, length.out = 400))
```

*Iteration 2* — `make_shrink_proposer(400, shrink = 2)` is called with the 400
rows of iteration 1. `determ_poly_end()` has just stored
`list(centers = 0.003904)` in the sideload. The observed range of the last
iteration is `[0.002, 0.006]`, so `spread = (0.006 - 0.002) / 2 / 2 = 0.001`,
and the new proposals span `[0.002904, 0.004904]`:

```
# A tibble: 400 x 1
   hiv.test.rate_1
             <dbl>
 1         0.00411
 2         0.00318
 3         0.00477
 ...
400         0.00352
```

(The values are a `seq()` of length 400 over the shrunk interval, `sample()`d so
that neighbouring `.proposal_index` values are not correlated.)

*Iteration 3* — the range of iteration 2 is `[0.002904, 0.004904]`, the new
center is `0.003851`, so `spread = 0.0005` and the proposals span
`[0.003351, 0.004351]`. The interval halves at every iteration.

### 3.4 Example — several parameters at once

Wave 2 / job 1: `params = paste0("hiv.trans.scale_", 1:3)` with
`make_proposer_se_range(400, retain_prop = 0.3)`. The proposer computes the
squared error over the 3 targets for every row of `results`, keeps the best 30 %,
takes the per-parameter range of that subset, and draws a Latin Hypercube inside
the resulting box:

```
# A tibble: 400 x 3
   hiv.trans.scale_1 hiv.trans.scale_2 hiv.trans.scale_3
               <dbl>             <dbl>             <dbl>
 1              2.31             0.394             0.352
 2              2.58             0.428             0.311
 3              2.44             0.371             0.338
 ...
400              2.49             0.410             0.327
```

Note the difference with `make_shrink_proposer()`: here the rows are a joint
design over the 3 parameters (the parameters interact), whereas the shrink
proposer builds each column independently.

### 3.5 Example — a naive custom proposer

A proposer does not have to be clever. This one is valid and self-contained:

```r
make_random_walk_proposer <- function(n_new, sd = 0.1) {
  force(n_new); force(sd)
  function(calib_object, job, results) {
    best <- swfcalib::get_default_proposal(calib_object)[, job$params]
    out <- lapply(job$params, function(p) {
      rnorm(n_new, mean = best[[p]], sd = sd * abs(best[[p]]))
    })
    names(out) <- job$params
    dplyr::as_tibble(out)
  }
}
```

Output for `job$params = c("a", "b")` and `n_new = 5`:

```
# A tibble: 5 x 2
       a     b
   <dbl> <dbl>
1  0.982  4.21
2  1.05   3.88
3  0.941  4.09
4  1.11   3.95
5  0.996  4.30
```

### 3.6 Proposer situations summary

| situation | expected output |
|-|-|
| iteration 1 | function not called — `job$initial_proposals` is used |
| job already converged | function not called — the value from `default_proposal` is used for every proposal |
| normal iteration | `n_sims` x `length(job$params)` tibble |
| all `results` rows are `NA` for the targets | drop them first; if nothing is left, fall back to a wide/initial range rather than producing `NaN` |
| the proposer needs history | store it with `save_sideload()`; `results` already holds every iteration of the wave, use `.iteration` to slice |
| the proposer cannot proceed | `stop()` with an explicit message — this aborts step 1 and the workflow (this is what `make_shrink_proposer()` does when no `centers` sideload exists) |

---

## 4. The end determination function — `get_result`

### 4.1 Contract

```r
get_result <- function(calib_object, job, results) {
  if (<not converged>) return(NULL)
  <one-row tibble of calibrated values>
}
```

**Exactly two valid return values:**

| return | meaning |
|-|-|
| `NULL` | the job is **not** finished — keep proposing and simulating |
| a **one-row** `tibble` / `data.frame` with one column per `job$params` | the job is finished, and these are the calibrated values |

Rules for the non-`NULL` case:

- **one row exactly.** `update_default_proposal()` does
  `default_proposal[, shared] <- result[, shared]`; more than one row corrupts
  the default proposal (or errors).
- **column names must match `job$params`.** Matching is done by
  `intersect(names(result), names(default_proposal))`; a misspelled name is
  **silently ignored**, and the parameter keeps its default value forever.
- extra columns that do exist in `default_proposal` *will* be written — only
  return what the job owns.
- no `NA` / `NaN` — these values become the final calibrated parameters.

### 4.2 Consequences of returning non-`NULL`

`update_calibration_state()` then:

1. marks the job done in `state$jobs_done` (`update_jobs_done_status()`);
2. writes the values into `state$default_proposal`
   (`update_default_proposal()`), so every subsequent proposal — of this wave and
   of all later waves — uses them;
3. if **all** jobs of the wave returned non-`NULL`, the wave is marked done and
   the calibration moves to the next wave (or finishes, writing
   `calibrated.csv` and `full_results.rds` in step 3).

### 4.3 Example — not converged

```r
get_result(calib_object, job, results)
#> NULL
```

That is the whole output. Iteration continues (until
`config$max_iteration`, at which point step 1 `stop()`s).

### 4.4 Example — converged

Single-parameter job (`determ_poly_end(0.001, poly_n = 5)` on
`hiv.test.rate_1`):

```
# A tibble: 1 x 1
  hiv.test.rate_1
            <dbl>
1         0.00385
```

Multi-parameter job (`determ_end_thresh(thresholds = rep(0.02, 3), n_enough = 100)`
on the transmission scalers):

```
# A tibble: 1 x 3
  hiv.trans.scale_1 hiv.trans.scale_2 hiv.trans.scale_3
              <dbl>             <dbl>             <dbl>
1              2.47             0.425             0.334
```

And the resulting `default_proposal` after the update:

```
# A tibble: 1 x 9
  hiv.test.rate_1 ... tx.init.rate_3 hiv.trans.scale_1 hiv.trans.scale_2 hiv.trans.scale_3
            <dbl>              <dbl>             <dbl>             <dbl>             <dbl>
1         0.00385              0.358              2.47             0.425             0.334
```

### 4.5 Important: `get_result` must be *stable*

`get_jobs_results()` calls `get_result` on **all** the jobs of the wave at every
iteration, converged ones included, and the wave is considered done only if
**no** job returns `NULL` in that same pass:

```r
done <- !any(vapply(job_results, is.null, logical(1)))
```

A job that converged at iteration 4 but returns `NULL` at iteration 5 **un-does
the wave** and blocks it. So a converged job must keep returning a non-`NULL`
result on every later iteration.

This is naturally satisfied by the built-ins because `results` only grows:

- `determ_end_thresh()` counts rows within the thresholds — that count is
  monotonically non-decreasing.
- `determ_poly_end()` keeps predicting from an increasingly dense sample around
  the solution; also, once the job is done it stops proposing, so its parameter
  column is constant on the new rows and the prediction barely moves.

If you write a custom end function that is not naturally monotonic, make it
idempotent explicitly:

```r
get_result <- function(calib_object, job, results) {
  sl <- swfcalib::load_sideload(calib_object, job)
  if (!is.null(sl$final)) return(sl$final)     # already converged: re-emit
  # ... convergence test ...
  if (!converged) return(NULL)
  out <- dplyr::as_tibble(setNames(as.list(values), job$params))
  swfcalib::save_sideload(calib_object, job, c(sl, list(final = out)))
  out
}
```

### 4.6 `get_result` situations summary

| situation | expected output |
|-|-|
| iteration 0 (nothing simulated yet) | not called at all |
| not enough / too noisy results | `NULL` |
| converged this iteration | 1-row tibble, columns = `job$params` |
| already converged in a previous iteration | the **same** 1-row tibble again (never `NULL`) |
| partially converged (some params ok, some not) | `NULL` — a job is all-or-nothing; split it into several jobs if the parameters are independent |
| it wants to hand information to the proposer | `save_sideload()` before returning, since `get_result` runs first |
| `max_iteration` reached without convergence | still `NULL`; `update_wave_iteration()` raises `"Max iteration exceded, stopping."` |

---

## 5. The built-ins, end to end

### 5.1 `make_shrink_proposer(n_new, shrink = 2)` (`R/proposers.R`)

- **Reads** `load_sideload(calib_object, job)$centers`, a numeric vector with one
  value per `job$params`, **in the same order**.
- **Errors** (`stop("No centers were provided for shrinkage, abort!")`) when the
  sideload is absent — so it must be paired with an end function that writes
  `centers`, i.e. `determ_poly_end()`.
- For each parameter: takes the range of the **last iteration only**, halves it
  (`/ shrink / 2` on each side of the center), clips the new interval to the
  observed range, lays `n_new` equally spaced points in it and shuffles them.
- **Returns** an `n_new` x `length(job$params)` tibble.

### 5.2 `make_proposer_se_range(n_new, retain_prop = 0.2)` (`R/proposers.R`)

- Computes `sum_j (outcome_j - target_j)^2` over **all iterations** of the wave.
- Keeps the best `ceiling(nrow * retain_prop)` rows, takes the per-parameter
  range of that subset, and draws `lhs::randomLHS(n_new, length(job$params))`
  rescaled to that box.
- **Returns** an `n_new` x `length(job$params)` tibble.
- **Constraint:** the loop that collects the ranges iterates over
  `seq_along(job$targets)` while indexing the parameter columns, so this proposer
  assumes `length(job$params) == length(job$targets)`. With fewer targets than
  parameters it errors; with more, the extra ranges are ignored.
- `NA` outcomes propagate into the score and then into the ranges — filter them
  out in the simulator or wrap this factory.

### 5.3 `determ_end_thresh(thresholds, n_enough)` (`R/determ_end.R`)

- Keeps the rows where `abs(outcome_j - target_j) < thresholds[j]` for **every**
  `j` (`thresholds` is one value per target, in `job$targets` order).
- If `nrow(kept) > n_enough`: returns the single row of parameters whose
  standardized distance to the per-column median is smallest —
  `which.min(rowSums(abs(x - median(x)) / sd(x)))`.
- Otherwise returns `NULL`.
- Writes **no** sideload, so it cannot drive `make_shrink_proposer()`.
- Edge case: if a retained parameter column is constant, `sd(x) == 0` yields
  `NaN` and `which.min()` may pick an arbitrary row.

### 5.4 `determ_poly_end(threshold, poly_n = 3)` (`R/determ_end.R`)

- **Pools** all `(param_i, target_i)` pairs into two long vectors, so it fits a
  *single* polynomial across all the parameters of the job. This is only valid
  when the parameter/outcome relationship is the same for every pair (e.g. the
  three race-specific `tx.init.rate` / `cc.linked1m` pairs); otherwise use one
  job per parameter.
- Drops incomplete rows, standardizes, fits `lm(value ~ poly(param, poly_n))`,
  and inverts it by `optimize()` to get the parameter value predicted to hit each
  target. Those values are `newp`.
- **Always** writes the sideload `list(centers = newp)` — including on the
  iteration where it returns `NULL`. That is what the shrink proposer consumes,
  and why the first call (no previous centers) returns `NULL` unconditionally.
- Returns the 1-row tibble of `newp` only when **both** conditions hold for
  every target: the prediction moved less than `threshold` since the previous
  iteration, **and** the predicted outcome is within `threshold` of the target.
- Because it overwrites `centers` at every iteration, the sideload always
  reflects the latest estimate.

### 5.5 Pairing summary

| proposer | compatible end function | why |
|-|-|-|
| `make_shrink_proposer()` | `determ_poly_end()` **only** | needs the `centers` sideload |
| `make_proposer_se_range()` | `determ_end_thresh()` (or `determ_poly_end()`) | uses no sideload |
| custom | any | as long as the sideload contract matches |

---

## 6. Checklist

**Proposer (`make_next_proposals`)**

- [ ] signature `function(calib_object, job, results)`
- [ ] returns a tibble, never `NULL`
- [ ] columns are exactly `job$params`, same names
- [ ] `config$n_sims` rows
- [ ] no `NA`, no `NaN`, no infinite values
- [ ] subsets `results` by `job$params` / `job$targets` before using it
- [ ] tolerates `NA` outcomes in `results`

**End determination (`get_result`)**

- [ ] signature `function(calib_object, job, results)`
- [ ] returns `NULL` **or** a 1-row tibble — nothing else
- [ ] column names match `job$params` **exactly** (a typo is silent)
- [ ] returns the same non-`NULL` result on every subsequent iteration
- [ ] writes any sideload the proposer needs, on every iteration, before
      returning
- [ ] returned values are plausible inputs for the simulator (they become the
      final calibrated parameters in `calibrated.csv`)
