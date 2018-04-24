source('tests/infer-test-init.R')
tinfer_consistent(t, dbor) 
tinfer_benchmark(t, dbor)


# no subset.
# expr      min         lq      mean     median        uq      max neval
# {     f = compute_joint(t, dbor) }  797.015   834.4085  1115.949   882.1815   955.659 102198.6  1000
# {     h = compute_log_joint(t, dbor) } 1096.743  1171.9735  3294.123  1280.9695  1444.470 103972.4  1000
# {     g = bnclassify:::compute_anb_log_joint_per_class(t, dbor) } 9335.735 11867.2435 23797.151 13246.5850 14307.619 135375.7  1000