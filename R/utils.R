select_test = function(chosen_test,rvs) {
  # changes all reactive values to 0 except for the selected proceudure
  # e.g. select_test('hgg', <reactive values>)
  test_names = c('awa','wmed','hgg','hlg','hssg')
  match = chosen_test == test_names
  
  for(ii in 1:length(test_names)) {
    if(match[ii]) {
      rvs[[ test_names[ii] ]] = 1
    } else {
      rvs[[ test_names[ii] ]] = 0
    }
  }
  
  return(rvs)
}

unselect_test = function(test,rvs) {
  rvs[[ test ]] = 0
  return(rvs)
}

clear_selections = function(rvs) {
  test_names = c('awa','wmed','hgg','hlg','hssg')
  
  for(ii in 1:length(test_names)) {
    rvs[[ test_names[ii] ]] = 0
  }
  
  return(rvs)
}
