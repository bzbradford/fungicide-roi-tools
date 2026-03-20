## Crop Protection Network Fungicide ROI Calculator

Return on investment (ROI) calculators are interactive tools designed to help users evaluate the potential economic benefit of specific agricultural practices or inputs. By combining research-based data with user-provided variables — such as input costs, market prices, and production outcomes — these calculators estimate expected net benefits and breakeven probabilities, both of which should be used in the decision-making process. ROI calculators are intended to support informed decision-making by offering insight into the financial risks and rewards of adopting a particular strategy under varying conditions. While not predictive, they provide a valuable framework for exploring scenarios based on real-world data.

The **[Crop Protection Network (CPN)](http://cropprotectionnetwork.org/) Fungicide ROI Calculator** is a research-based decision-support tool developed by extension experts. It assists farmers, agronomists, and agricultural professionals with exploring the potential economic return of fungicide applications in crop production. Drawing on data from multi-year, university-conducted fungicide trials across the U.S. and Ontario, Canada, this calculator estimates net benefits and breakeven probabilities based on user-defined inputs such as treatment cost, disease severity, and expected commodity prices.

### How To Use

1. Select your crop using the tabs at the top
2. Enter your expected yield and sale price
3. Select the anticipated late season disease severity level
4. Adjust treatment costs to local pricing or leave at U.S. median defaults
5. Review the results in the charts and data table

### Key Metrics

- **Expected net benefit:** Estimated profit or loss: `yield_benefit * sale_price − treatment_cost`
  * Where: `treatment_cost = program_cost + application_cost * number_of_applications`
- **Breakeven probability:** Probability that applying the fungicide yields positive net benefit
- **Breakeven cost:** Maximum program + application cost that yields a 50% or greated break even probability based on the expected benefit of the program and expected sale price.
- **95% confidence intervals:** Range of plausible values based on model uncertainty

### Interpreting Results

- Programs are ranked by expected net benefit (highest first)
- Higher ranked programs are expected to be more likely to increase yield and return a profit, but real-world results may vary
- Error bars (when shown) indicate the 95% confidence interval for the expected net benefit calculation
- Negative values (red shaded area) indicate expected loss for the selected conditions and input prices

## Methodology

### How Were Fungicide Products Selected?

For each crop system, a coordinating working group develops annual testing protocols based on feedback from university extension specialists. Trials typically test multiple fungicide products per year, per trial. Products are selected based on availability and market share, and typically represent products available to and used by a majority of farmers. Focus is also placed on newer products where efficacy data may be lacking across major production regions of the United States.

### How Were Treatment Costs Determined?

The coordinating working group for each crop solicits information on fungicide product pricing and application costs from university extension specialists annually. Methods for collecting these costs vary by state and year. Specialists may collect pricing information from surveys or direct feedback from university Extension, industry, farmers, and other agricultural personnel. Product and application costs are averaged, and a national mean and median are determined for each product based on submitted data. The treatment costs for each product listed in the calculator can be changed to tailor the expected benefit estimates to local pricing.

### How Are Expected Net Benefits Calculated?

The expected net benefit is an estimate of ROI when considering several variable factors that can be measured or estimated. These factors include the proportional yield benefit (yield of treated plots vs. non-treated) for each fungicide across a range of disease levels. This is then combined with total treatment cost (cost of the fungicide plus application cost) and expected commodity sale price, with both parameters fit to a function that estimates the expected net benefit per acre. This function is only a best estimate based on available data and reported, real-world treatment costs. Unaccountable error and uncertainty exist in the data, and actual ROI may occasionally differ significantly from the estimates reported here.

### How Are Breakeven Probabilities Calculated?

Thousands of simulations were performed to estimate the expected ROI of fungicide applications across a range of inputs used to calculate expected net benefits. The expected breakeven probabilities represent the proportion of total simulations that had an expected net benefit of zero or greater, given a fungicide and set of crop characteristic inputs.

## More Information

### Credits

- **Project leads:** Damon Smith, Department of Plant Pathology, University of Wisconsin-Madison; Daren Mueller, Department of Plant Pathology, Entomology and Microbiology, Iowa State; Kiersten Wise, Department of Plant Pathology, University of Kentucky.
- **Data compilation and economic analysis:** Maria Oros, Data Science Institute, University of Wisconsin-Madison, with contributions from Isaac Baumann and Jason Lo, also at DSI.
- **App development:** Initial calculators developed by Maria Oros, current app developed by Ben Bradford, UW-Madison Department of Entomology.
- Additional contributors are listed within each tool.

### Funding sources

Funding provided in part by the USDA ARS through the National Predictive Modeling Tool Initiative (NPMTI), North Central Soybean Research Program, the Crop Protection Pest Management program which funds the North Central Integrated Pest Management Center, and the University of Wisconsin-Madison.

### Disclaimer

This information is only a guide, but is based on multi-year research across multiple locations. Contributors and data managers assume no liability resulting from the use of these estimates.

References to products in this resource are not intended to be an endorsement to the exclusion of others that may be similar. Individuals using such products assume responsibility for their use in accordance with current directions of the manufacturer.

Any opinions, findings, conclusions, or recommendations expressed in this publication are those of the author(s) and should not be construed to represent any official USDA or U.S. Government determination or policy.

### About the Crop Protection Network

The Crop Protection Network (CPN) is a multi-state and international collaboration of university and provincial extension specialists, and public and private professionals who provide unbiased, research-based information to farmers and agricultural personnel. Our goal is to communicate relevant information that will help professionals identify and manage field crop diseases.

Find more crop protection resources at the [Crop Protection Network](https://cropprotectionnetwork.org/).

### USDA Civil Rights Statement

In accordance with Federal civil rights law and U.S. Department of Agriculture (USDA) civil rights regulations and policies, the USDA, its Agencies, offices, and employees, and institutions participating in or administering USDA programs are prohibited from discriminating based on race, color, national origin, religion, sex, disability, age, marital status, family/parental status, income derived from a public assistance program, political beliefs, or reprisal or retaliation for prior civil rights activity, in any program or activity conducted or funded by USDA (not all bases apply to all programs).

Persons with disabilities who require alternative means of communication for program information (e.g., Braille, large print, audiotape, American Sign Language, etc.) should contact the State or local Agency that administers the program or contact USDA through the Telecommunications Relay Service at 711 (voice and TTY). Additionally, program information may be made available in languages other than English.

To file a program discrimination complaint, complete the USDA Program Discrimination Complaint Form, AD-3027, found online at [How to File a Program Discrimination Complaint](https://www.usda.gov/oascr/how-to-file-a-program-discrimination-complaint) and at any USDA office, or write a letter addressed to USDA and provide in the letter all of the information requested in the form. To request a copy of the complaint form, call (866) 632-9992. Submit your completed form or letter to USDA by: (1) mail: U.S. Department of Agriculture, Office of the Assistant Secretary for Civil Rights, 1400 Independence Avenue, SW, Mail Stop 9410, Washington, D.C. 20250-9410; (2) fax: (202) 690-7442; or (3) email: [program.intake@usda.gov](mailto:program.intake@usda.gov).

USDA is an equal opportunity provider, employer, and lender.

### Source code

[View source code on GitHub](https://github.com/bzbradford/fungicide-roi-tools)

### Copyright

©2026 by the Crop Protection Network. All rights reserved.
