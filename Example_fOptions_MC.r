     ## SOURCE("fOptions.D2-MonteCarloOptions")

     ## How to perform a Monte Carlo Simulation?

     ## First Step:
        xmpOptions("\nStart: Sobol Generator > ")
        # Write a function to generate the option's innovations.
        # Use scrambled normal Sobol numbers:
        sobolInnovations = function(mcSteps, pathLength, init, ...) {
          # Create Normal Sobol Innovations:
          innovations = rnorm.sobol(mcSteps, pathLength, init, ...)
          # Return Value:
          innovations }

     ## Second Step:
        xmpOptions("\nNext: Wiener Path > ")
        # Write a function to generate the option's price paths.
        # Use a Wiener path:
        wienerPath = function(eps) {
          # Note, the option parameters must be globally defined!
          # Generate the Paths:
          path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
          # Return Value:
          path }

     ## Third Step:
        # Write a function for the option's payoff

        # Example 1: use the payoff for a plain Vanilla Call or Put:
        xmpOptions("\nNext: Plain Vanilla Payoff > ")
        plainVanillaPayoff = function(path) {
          # Note, the option parameters must be globally defined!
          # Compute the Call/Put Payoff Value:
          ST = S*exp(sum(path))
          if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
          if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
          # Return Value:
          payoff }

        # Example 2: use the payoff for an arithmetic Asian Call or Put:
        xmpOptions("\nNext: Arithmetic Asian Payoff > ")
        arithmeticAsianPayoff = function(path) {
          # Note, the option parameters must be globally defined!
          # Compute the Call/Put Payoff Value:
          SM = mean(S*exp(cumsum(path)))
          if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
          if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
          # Return Value:
          payoff }

     ## Final Step:
        xmpOptions("\nNext: Set Option Parameters > ")
        # Set Global Parameters for the plain Vanilla / arithmetic Asian Options:
        TypeFlag <<- "c"; S <<- 100; X <<- 100
        Time <<- 1/12; sigma <<- 0.4; r <<- 0.10; b <<- 0.1

        # Do the Asian Simulation with scrambled random numbers:
        xmpOptions("\nNext: Monte Carlo Simulation > ")
        mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 5000,
          mcLoops = 50, init = TRUE, innovations.gen = sobolInnovations,
          path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff,
          antithetic = TRUE, standardization = FALSE, trace = TRUE,
          scrambling = 2, seed = 4711)

        # Plot the MC Iteration Path:
        xmpOptions("\nNext: Output Results > ")
        par(mfrow = c(1, 1))
        mcPrice = cumsum(mc)/(1:length(mc))
        plot(mcPrice, type = "l", main = "Arithmetic Asian Option",
          xlab = "Monte Carlo Loops", ylab = "Option Price")

        # Compare with Turnbull-Wakeman Approximation:
        TW = TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = 100, SA = 100,
          X = 100, Time = 1/12, time = 1/12, tau = 0 , r = 0.1, b = 0.1,
          sigma = 0.4)$price
        print(TW)
        abline(h = TW, col = 2)



