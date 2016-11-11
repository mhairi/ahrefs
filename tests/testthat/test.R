context('Do we have the expected response')

test_that('Basic call works', {
  url <- 'www.google.com'
  api_key <- '1f5295f58ebd7f5622c803adf3bf40ce16139a6f'

  rank          <- get_rank(url, api_key)
  domain_rating <- get_domain_rating(url, api_key)
  ref_domains   <- get_refering_domains(url, api_key)

  expect_is(rank, 'integer')
  expect_is(domain_rating, 'integer')
  expect_is(ref_domains, 'list')
})
