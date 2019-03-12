context("Quasi Poisson")


# Create a sample GLM and GLM summary
data(dataCar, package = "insuranceData")
dataCarGLM <- glm(numclaims ~ veh_value + veh_age + gender + agecat,
                  data = dataCar, family = quasi_poisson, offset = log(exposure), x = TRUE)
model_AIC_BIC <- quasi_poisson_aic_bic(dataCarGLM)
dataCarGLMSummary <- summary(dataCarGLM)


# Test quasi_poisson AIC
test_that("quasi_poisson AIC", {
  expect_equal(AIC(dataCarGLM),
               34842.8257899163)
  expect_equal(BIC(dataCarGLM),
               34888.4515053749)
  expect_equal(dataCarGLMSummary$aic,
               34842.8257899163)
})


# Test quasi_poisson_aic_bic output
test_that("quasi_poisson_aic_bic output", {
  expect_equal(model_AIC_BIC$AIC, dataCarGLM$aic)
  expect_equal(model_AIC_BIC$AIC, AIC(dataCarGLM))
  expect_equal(model_AIC_BIC$BIC, BIC(dataCarGLM))
})
