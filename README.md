# TeenyTT

`TeenyTT` is a small interactive proof assistant, meant to be used as a didactic implementation.

## Project Structure
The main meat of the proof assistant lives inside of ./src/TeenyTT/Core/, which includes
- The [Evaluator](./src/TeenyTT/Core/Eval.hs)
- The [Quoter](./src/TeenyTT/Core/Quote.hs)
- The [Conversion Checker](./src/TeenyTT/Core/Conversion.hs)

The various implementations of refiner tactics can be found inside of ./src/TeenyTT/Core/Refiner/
