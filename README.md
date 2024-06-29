# ukhls_class
Code to create respondent occupational class for the UK Household Longitudinal Study panel (UKHLS).

This code generates respondent class according to NS-SeC methodology (Office for National Statistics), using information across the whole UKHLS panel, including information from the earlier British Household Panel Study (BHPS). By default, the UKHLS does not provide this, and most cases are missing across waves. Up-to-date for Wave 9 (2018). No loops because the conditions differ by wave.

Creates an 8-class version of the NS-SeC categorisation.
