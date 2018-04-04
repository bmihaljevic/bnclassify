// R and C++
// most code could be in R, with certain critical parts in C++
// Thus greedy could be in R, but parts where I update the prediction matrix output or similar could be C++
// The data could be a integer matrix rather than a data frame, because all entries are integers
// maybe distinguish train set and test set?
  // yes, in greedy distinguish the full data set

// Commonalities:
// the data set is common across iterations
// many of the cpts are common
// the thetas corresponding to the data are common as long as the cpts are fixed
// many times the product is fixed given the same thetas
// **check that factor levels correspond to cpt levels; this is also done initially, at this first point, so the accessing code can be safe.**  

// DAtaset initial optimize
 // Do all entries - 1. 
 // Arrange by instance, not by column?
 // Then do the splitting into the folds 
 
 // predict_rcpp
//   Model model(x);
//   // cpts to log. or not? 
//   Testdata test(newdata); 
//   // make sure levels match the levels in my data set.
//   MappedModel mm(model, test);  
// model: has features, class, etc. that is independent of the dataset. 
// // [[Rcpp::export]]
// NumericVector make_cpt(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, DataFrame df) { 
//   Testdata ds(df, features);
//   CPT c = CPT(cpt, features, class_var, ds); 
//   IntegerVector inds = IntegerVector::create(1);
//   inds[0] = 2;
//   // must initialize vector  of entries
//   std::vector<double> entries(2);
//   c.get_entries(inds, entries);
//   return wrap(entries);
//   // return NumericVector::create(1);
// }
// 
// //[[Rcpp::export]]
// IntegerMatrix df2matrix(DataFrame x) {
//   IntegerMatrix y = internal::convert_using_rfunction(x, "data.matrix");  
//   return y;
// }
// 
// //[[Rcpp::export]]
// NumericVector get_instance(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, DataFrame df) { 
//   Testdata ds(df, features);
//   CPT c = CPT(cpt, features, class_var, ds);
//   IntegerMatrix mat = df2matrix(df);
//   IntegerVector row = mat.row(1);
//   std::vector<double> entries(2);
//   c.get_entries(row, entries);
//   return wrap(entries);
// }
// 
// //[[Rcpp::export]]
// NumericVector get_row(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, DataFrame df) { 
//   Testdata ds(df, features);
//   CPT c = CPT(cpt, features, class_var, ds);
//   // std::vector<double> entries(2);
//   // c.get_entries(1, entries);
//   // return wrap(entries);
//   return NumericVector::create(1);
// }  
// 
// // 
// // // [[Rcpp::export]] 
// // NumericVector compute_joint_instance(MappedModel model) {
// //  // from 1 to N get the cpt entries of all classes from the cpts; multiply them.
// //  // get all cpts; for all get the entries, then multiply the entries to get the row of output
// //  // output could simply be a vector, thus do not increase to output 
// //  // i could have iterators over the data and just go across it 
// //  return NumericVector::create(1); 
// // }
// 
// // output: N x nclass
// // instances: N x whatever. Follow the feature indices.  
//  
// // NumericVector compute_joint(MappedModel model) {  
// 
// // [[Rcpp::export]]
// NumericVector do_mapped(List x, DataFrame newdata) {
//  Model model(x);
//  Testdata test(newdata, model.getFeatures());
//  MappedModel mm(model, test);
//  // CPT c = mm.get_mapped_cpt(0);
//  // std::vector<double> entries(2);
//  // c.get_entries(1, entries);
//  // return wrap(entries);
//  return NumericVector::create(1);
// }
// 
// 
// 
// /*** R   
// kr <- foreign::read.arff('~/gd/phd/code/works-aug-semi-bayes/data/original/kr-vs-kp.arff')
// library(bnclassify)
// dbor <- kr
// t <- lp(nb('class', dbor), dbor, smooth = 1)
// make_cpt(t$.params$bkblk, features(t), class_var(t), dbor)
// get_instance(t$.params$bkblk, features(t), class_var(t), dbor)
// get_row(t$.params$bkblk, features(t), class_var(t), dbor)
// t$.params$bkblk
// # get_dataset(dbor, 0, 0)
// # get_dataset(dbor, 36, 0)
// # # get_dataset(dbor, 37, 0) # check out of
// do_mapped(t, dbor)
// # microbenchmark::microbenchmark(
// #                                { d = get_row(cpt, f, cvar, dbor)  },
// #                                {a = make_cpt(cpt, f, cvar, dbor)},
// #                                { b = get_instance(cpt, f, cvar,  dbor)  },
// #                                { e = get_dataset(dbor, 0, 25) },
// #                                { g = do_mapped(t, dbor)})  
// **/
//   

