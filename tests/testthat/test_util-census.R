library(visdom)
library(acs)
context("Integration test for merging census dataframes")

check_acs_key = function() {
     if( ! acs::api.key.exists() ) {
          skip(paste(
               "A census API key has not been registered with the acs package. Skipping test.",
               "To install a census API key, go to http://api.census.gov/data/key_signup.html",
               "To register a census API key, do: acs::api.key.install(key='PASTE_YOUR_KEY_HERE')"))
     }
}

test_that("acs downloading & mergeCensus()", {
     check_acs_key()
     geography = c(acs::geo.make(zip.code='94709'), acs::geo.make(zip.code='94710'))
     censusStats = loadACS(geography = geography)
     df = data.frame("ZCTA" = as.numeric(censusStats$ZCTA), "fake_column" = as.numeric(censusStats$mean_fam_income) / 2.0)
     df = mergeCensus(df, censusStats=censusStats)
     expect_equal(df$fake_column, as.numeric(df$mean_fam_income) / 2.0)
})

test_that("acs cache speedup", {
     check_acs_key()
     old_cache_path=R.cache::getCacheRootPath()
     cache_dir = file.path(tempdir(), "visdom_test_R.cache")
     dir.create(cache_dir, recursive=T, showWarnings=F)
     R.cache::setCacheRootPath(cache_dir)
     geography = c(acs::geo.make(zip.code='94709'), acs::geo.make(zip.code='94710'))
     time1 = system.time(loadACS(geography=geography))
     time2 = system.time(loadACS(geography=geography))
     expect_gt(time1['elapsed'], time2['elapsed'])
     # Clean up
     unlink("cache_dir", recursive=TRUE)
     R.cache::setCacheRootPath(old_cache_path)
})
