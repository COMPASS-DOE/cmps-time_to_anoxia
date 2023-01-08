# Soil Characterization Data for Old Woman Creek (OWC)

Data from the **Soil Characterization** experiment.
More details, data, scripts, etc. available [here](https://github.com/COMPASS-DOE/cmps-soil_characterization).

**1. owc_characterization**

This file contains all the chemistry data, compiled, for OWC. 
Data are in longform. 


**2. icr_long_all_samples_owc**

This file contains the processed FT-ICR-MS data for OWC samples, for *all replicates*.  
Use this for statistics, spatial variability, etc. Molecules/peaks included are peaks present in the samples. 


**3. icr_long_treatments_owc**


This file contains the processed FT-ICR-MS data for OWC samples, *summarized for each treatment*.  
Use this for Van Krevelen plots, comparison among treatments, etc. Molecules/peaks included are peaks present in the samples. 


**4. icr_meta**

This file contains the FT-ICR-MS related metadata for the peaks identified. Columnns include elemental composition for each peak, H:C and O:C ratios, NOSC, AImod, and other indices. 


**5. icr_report_001_0176**

This file contains the unprocessed report for the WLE FT-ICR-MS data.  


**6. sample_key**

This file contains the sample key for all Synoptic Sites. Use the `sample_label` column to match results, if needed. 