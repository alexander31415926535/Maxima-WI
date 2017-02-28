#!/usr/bin/runghc 

{-# LANGUAGE MultiWayIf #-}
-- * Mconsole
-- This module attempts to introduce unicode goodies into maxima text console. things like x^2 are written in proper unicode.

-- * Imports
module Mconsole where

import Data.Attoparsec.ByteString.Char8 
import Data.ByteString.Char8 (pack) 
import Maxima
import Data.List.Extra hiding (any)
import System.Console.Haskeline
import Control.Monad.Trans (lift)

-- * tounicode function
tounicode :: String -> String
tounicode str = foldl1 (.) (zipWith replace  ("*":terms) ("·":helper terms)) str
  where terms = case parseOnly allpowers (pack str) of
                 Left _     -> []
                 Right pstr -> pstr

        helper xp = map (foldl1 (.) (zipWith  replace ["^","1","2","3","4","5","6","7","8","9","0"]
                                                      ["","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹","⁰"])) xp

(<^>) = flip (<?>)              -- more convenient help combinator

powerp :: Parser String
powerp = "Powerp"  <^> ((:) <$> char '^' <*> many1 digit)

allpowers :: Parser [String]
allpowers = "allpowers"  <^> many' (takeTill (== '^') *> powerp)

-- * main             

-- oldmain = runMaxima 4424 maximaPrompt

main :: IO ()
main = do
  putStrLn "Maxima\nDedicated to the memory of William Schelter"
  runMaxima 4424 haskelinemod

haskelinemod srv = runInputT (setComplete mcompletion defaultSettings) loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just "help"  -> do
         lift $ listcommands
         loop
       Just input -> do
         answer <- lift $ askMaxima srv input
         let ans = tounicode (if length answer > 4 then init (drop 2 answer) else answer) -- XXX: ugly code to remove " \n" on both ends.
         outputStrLn $ ans
         loop

listcommands :: IO ()
listcommands = do
  mapM_ putStrLn ["Following commands are active:",
                  "help",
                  "quit"]

-- * Completion, big lists

mcompletion :: CompletionFunc IO
mcompletion = completeWord Nothing [' '] helper
  where
    helper :: String -> IO [Completion]
    helper x | length x >= 1 = return (map simpleCompletion (filter (isPrefixOf x) commlistE))
             | otherwise = return [simpleCompletion "help"]


commlistE :: [String]
commlistE = [
  "example",
  "exp",
  "expand",
  "expandwrt",
  "expandwrt_denom",
  "expandwrt_factored",
  "expintegral_chi",
  "expintegral_ci",
  "expintegral_e",
  "expintegral_e1",
  "expintegral_e_simplify",
  "expintegral_ei",
  "expintegral_li",
  "expintegral_shi",
  "expintegral_si",
  "expintexpand",
  "expintrep",
  "explicit",
  "explose",
  "expon",
  "exponentialize",
  "exponentialize",
  "expop",
  "express",
  "expt",
  "exptdispflag",
  "exptisolate",
  "exptsubst",
  "exsec",
  "extdiff",
  "extract_linear_equations",
  "extremal_subset",
  "ezgcd",
  "background",
  "background_color",
  "backslash",
  "backsubst",
  "backtrace",
  "bars",
  "barsplot",
  "barsplot_description",
  "base64",
  "base64_decode",
  "bashindices",
  "batch",
  "batchload",
  "bc2",
  "bdvac",
  "belln",
  "benefit_cost",
  "berlefact",
  "bern",
  "bernpoly",
  "bernstein_approx",
  "bernstein_expand",
  "bernstein_explicit",
  "bernstein_poly",
  "bessel_i",
  "bessel_j",
  "bessel_k",
  "bessel_simplify",
  "bessel_y",
  "besselexpand",
  "beta",
  "beta_args_sum_to_integer",
  "beta_expand",
  "beta_incomplete",
  "beta_incomplete_generalized",
  "beta_incomplete_regularized",
  "bezout",
  "bf_fft",
  "bf_find_root",
  "bf_find_root",
  "bf_fmin_cobyla",
  "bf_inverse_fft",
  "bf_inverse_real_fft",
  "bf_real_fft",
  "bfallroots",
  "bffac",
  "bfhzeta",
  "bfloat",
  "bfloatp",
  "bfpsi",
  "bfpsi0",
  "bftorat",
  "bftrunc",
  "bfzeta",
  "biconnected_components",
  "bimetric",
  "bindtest",
  "binomial",
  "bipartition",
  "bit_and",
  "bit_length",
  "bit_lsh",
  "bit_not",
  "bit_onep",
  "bit_or",
  "bit_rsh",
  "bit_xor",
  "block",
  "blockmatrixp",
  "bode_gain",
  "bode_phase",
  "border",
  "bothcoef",
  "boundaries_array",
  "box",
  "boxchar",
  "boxplot",
  "boxplot_description",
  "break",
  "breakup",
  "bug_report",
  "build_info",
  "build_sample",
  "buildq",
  "burn",
  "cabs",
  "canform",
  "canten",
  "capping",
  "capping",
  "cardinality",
  "carg",
  "cartan",
  "cartesian_product",
  "catch",
  "cauchy_matrix",
  "cauchysum",
  "cbffac",
  "cbrange",
  "cbtics",
  "cdf_bernoulli",
  "cdf_beta",
  "cdf_binomial",
  "cdf_cauchy",
  "cdf_chi2",
  "cdf_continuous_uniform",
  "cdf_discrete_uniform",
  "cdf_exp",
  "cdf_f",
  "cdf_gamma",
  "cdf_general_finite_discrete",
  "cdf_geometric",
  "cdf_gumbel",
  "cdf_hypergeometric",
  "cdf_laplace",
  "cdf_logistic",
  "cdf_lognormal",
  "cdf_negative_binomial",
  "cdf_noncentral_chi2",
  "cdf_noncentral_student_t",
  "cdf_normal",
  "cdf_pareto",
  "cdf_poisson",
  "cdf_rank_sum",
  "cdf_rayleigh",
  "cdf_signed_rank",
  "cdf_student_t",
  "cdf_weibull",
  "cdisplay",
  "ceiling",
  "center",
  "central_moment",
  "cequal",
  "cequalignore",
  "cf",
  "cfdisrep",
  "cfexpand",
  "cflength",
  "cframe_flag",
  "cgeodesic",
  "cgreaterp",
  "cgreaterpignore",
  "changename",
  "changevar",
  "chaosgame",
  "charat",
  "charfun",
  "charfun2",
  "charlist",
  "charp",
  "charpoly",
  "chdir",
  "chebyshev_t",
  "chebyshev_u",
  "check_overlaps",
  "checkdiv",
  "chinese",
  "cholesky",
  "christof",
  "chromatic_index",
  "chromatic_number",
  "cint",
  "circulant_graph",
  "clear_edge_weight",
  "clear_rules",
  "clear_vertex_label",
  "clebsch_gordan",
  "clebsch_graph",
  "clessp",
  "clesspignore",
  "close",
  "closefile",
  "cmetric",
  "cnonmet_flag",
  "coeff",
  "coefmatrix",
  "cograd",
  "col",
  "collapse",
  "collectterms",
  "color",
  "color",
  "color",
  "color_bar",
  "color_bar_tics",
  "colorbox",
  "columnop",
  "columns",
  "columnspace",
  "columnswap",
  "columnvector",
  "combination",
  "combine",
  "commutative",
  "comp2pui",
  "compare",
  "compfile",
  "compile",
  "compile_file",
  "complement_graph",
  "complete_bipartite_graph",
  "complete_graph",
  "complex",
  "complex_number_p",
  "components",
  "compose_functions",
  "concan",
  "concat",
  "cone",
  "conjugate",
  "conmetderiv",
  "connect_vertices",
  "connected_components",
  "cons",
  "constant",
  "constantp",
  "constituent",
  "constvalue",
  "cont2part",
  "content",
  "context",
  "contexts",
  "continuous_freq",
  "contortion",
  "contour",
  "contour_levels",
  "contour_plot",
  "contract",
  "contract",
  "contract_edge",
  "contragrad",
  "contrib_ode",
  "convert",
  "coord",
  "copy",
  "copy_file",
  "copy_graph",
  "copylist",
  "copymatrix",
  "cor",
  "cos",
  "cosh",
  "cosnpiflag",
  "cot",
  "coth",
  "cov",
  "cov1",
  "covdiff",
  "covect",
  "covers",
  "crc24sum",
  "create_graph",
  "create_list",
  "csc",
  "csch",
  "csetup",
  "cspline",
  "ct_coords",
  "ct_coordsys",
  "ctaylor",
  "ctaypov",
  "ctaypt",
  "ctayswitch",
  "ctayvar",
  "ctorsion_flag",
  "ctransform",
  "ctranspose",
  "ctrgsimp",
  "cube",
  "cube_graph",
  "cuboctahedron_graph",
  "current_let_rule_package",
  "cv",
  "cycle_digraph",
  "cycle_graph",
  "cylinder",
  "cylindrical",
  "abasep",
  "abs",
  "absboxchar",
  "absint",
  "absolute_real_time",
  "acos",
  "acosh",
  "acot",
  "acoth",
  "acsc",
  "acsch",
  "activate",
  "activecontexts",
  "adapt_depth",
  "adapt_depth",
  "add_edge",
  "add_edges",
  "add_vertex",
  "add_vertices",
  "addcol",
  "additive",
  "addmatrices",
  "addrow",
  "adim",
  "adjacency_matrix",
  "adjoin",
  "adjoint",
  "adjust_external_format",
  "af",
  "aform",
  "agd",
  "airy_ai",
  "airy_bi",
  "airy_dai",
  "airy_dbi",
  "alg_type",
  "algebraic",
  "algepsilon",
  "algexact",
  "algsys",
  "alias",
  "aliases",
  "all_dotsimp_denoms",
  "allbut",
  "allocation",
  "allroots",
  "allsym",
  "alphabetic",
  "alphacharp",
  "alphanumericp",
  "amortization",
  "and",
  "animation",
  "annuity_fv",
  "annuity_pv",
  "antid",
  "antidiff",
  "AntiDifference",
  "antisymmetric",
  "append",
  "appendfile",
  "apply",
  "apply1",
  "apply2",
  "applyb1",
  "apropos",
  "args",
  "arit_amortization",
  "arithmetic",
  "arithsum",
  "array",
  "arrayapply",
  "arrayinfo",
  "arraymake",
  "arrays",
  "arraysetapply",
  "ascii",
  "asec",
  "asech",
  "asin",
  "asinh",
  "askexp",
  "askinteger",
  "asksign",
  "assoc",
  "assoc_legendre_p",
  "assoc_legendre_q",
  "assume",
  "assume_external_byte_order",
  "assume_pos",
  "assume_pos_pred",
  "assumescalar",
  "asymbol",
  "asympa",
  "at",
  "atan",
  "atan2",
  "atanh",
  "atensimp",
  "atom",
  "atomgrad",
  "atrig1",
  "atvalue",
  "augcoefmatrix",
  "augmented_lagrangian_method",
  "av",
  "average_degree",
  "axes",
  "axis_3d",
  "axis_bottom",
  "axis_left",
  "axis_right",
  "axis_top",
  "azimuth",
  "azimuth",
  "data_file_name",
  "days360",
  "dblint",
  "deactivate",
  "debugmode",
  "declare",
  "declare_constvalue",
  "declare_dimensions",
  "declare_fundamental_dimensions",
  "declare_fundamental_units",
  "declare_qty",
  "declare_translated",
  "declare_unit_conversion",
  "declare_units",
  "declare_weights",
  "decreasing",
  "decsym",
  "default_let_rule_package",
  "defcon",
  "define",
  "define_alt_display",
  "define_variable",
  "defint",
  "defmatch",
  "defrule",
  "defstruct",
  "deftaylor",
  "degree_sequence",
  "del",
  "delay",
  "delete",
  "delete_file",
  "deleten",
  "delta",
  "demo",
  "demoivre",
  "demoivre",
  "denom",
  "dependencies",
  "dependencies",
  "depends",
  "derivabbrev",
  "derivdegree",
  "derivlist",
  "derivsubst",
  "describe",
  "desolve",
  "determinant",
  "detout",
  "dfloat",
  "dgauss_a",
  "dgauss_b",
  "dgeev",
  "dgemm",
  "dgeqrf",
  "dgesv",
  "dgesvd",
  "diag",
  "diag_matrix",
  "diagmatrix",
  "diagmatrixp",
  "diagmetric",
  "diameter",
  "diff",
  "diff",
  "digitcharp",
  "dim",
  "dimacs_export",
  "dimacs_import",
  "dimension",
  "dimensionless",
  "dimensions",
  "dimensions_as_list",
  "direct",
  "directory",
  "discrete_freq",
  "disjoin",
  "disjointp",
  "disolate",
  "disp",
  "dispcon",
  "dispflag",
  "dispform",
  "dispfun",
  "dispJordan",
  "display",
  "display2d",
  "display_format_internal",
  "disprule",
  "dispterms",
  "distrib",
  "distribute_over",
  "divide",
  "divisors",
  "divsum",
  "dkummer_m",
  "dkummer_u",
  "dlange",
  "do",
  "doallmxops",
  "dodecahedron_graph",
  "domain",
  "domxexpt",
  "domxmxops",
  "domxnctimes",
  "dontfactor",
  "doscmxops",
  "doscmxplus",
  "dot0nscsimp",
  "dot0simp",
  "dot1simp",
  "dotassoc",
  "dotconstrules",
  "dotdistrib",
  "dotexptsimp",
  "dotident",
  "dotproduct",
  "dotscrules",
  "dotsimp",
  "dpart",
  "draw",
  "draw2d",
  "draw3d",
  "draw_file",
  "draw_graph",
  "draw_graph_program",
  "draw_realpart",
  "drawdf",
  "dscalar",
  "dscalar",
  "f90",
  "facexpand",
  "facsum",
  "facsum_combine",
  "factcomb",
  "factlim",
  "factor",
  "factorfacsum",
  "factorflag",
  "factorial",
  "factorial_expand",
  "factorout",
  "factors_only",
  "factorsum",
  "facts",
  "false",
  "fast_central_elements",
  "fast_linsolve",
  "fasttimes",
  "fb",
  "feature",
  "featurep",
  "features",
  "fernfale",
  "fft",
  "fib",
  "fibtophi",
  "fifth",
  "file_name",
  "file_name",
  "file_output_append",
  "file_search",
  "file_search_demo",
  "file_search_lisp",
  "file_search_maxima",
  "file_search_tests",
  "file_search_usage",
  "file_type",
  "file_type_lisp",
  "file_type_maxima",
  "filename_merge",
  "fill_color",
  "fill_density",
  "fillarray",
  "filled_func",
  "find_root",
  "find_root",
  "find_root_abs",
  "find_root_error",
  "find_root_rel",
  "findde",
  "first",
  "fix",
  "fixed_vertices",
  "flatten",
  "flength",
  "flipflag",
  "float",
  "float2bf",
  "floatnump",
  "floor",
  "flower_snark",
  "flush",
  "flush1deriv",
  "flush_output",
  "flushd",
  "flushnd",
  "fmin_cobyla",
  "font",
  "font_size",
  "for",
  "forget",
  "fortindent",
  "fortran",
  "fortspaces",
  "fourcos",
  "fourexpand",
  "fourier",
  "fourier_elim",
  "fourint",
  "fourintcos",
  "fourintsin",
  "foursimp",
  "foursin",
  "fourth",
  "fposition",
  "fpprec",
  "fpprintprec",
  "frame_bracket",
  "freeof",
  "freshline",
  "fresnel_c",
  "fresnel_s",
  "from_adjacency_matrix",
  "frucht_graph",
  "full_listify",
  "fullmap",
  "fullmapl",
  "fullratsimp",
  "fullratsubst",
  "fullsetify",
  "funcsolve",
  "functions",
  "fundamental_dimensions",
  "fundamental_units",
  "fundef",
  "funmake",
  "funp",
  "fv",
  "gamma",
  "gamma_expand",
  "gamma_greek",
  "gamma_incomplete",
  "gamma_incomplete_generalized",
  "gamma_incomplete_regularized",
  "gammalim",
  "gauss_a",
  "gauss_b",
  "gaussprob",
  "gcd",
  "gcdex",
  "gcdivide",
  "gcfac",
  "gcfactor",
  "gd",
  "gdet",
  "gen_laguerre",
  "generalized_lambert_w",
  "genfact",
  "genindex",
  "genmatrix",
  "gensumnum",
  "gensym",
  "geo_amortization",
  "geo_annuity_fv",
  "geo_annuity_pv",
  "geomap",
  "geometric",
  "geometric_mean",
  "geosum",
  "get",
  "get_edge_weight",
  "get_lu_factors",
  "get_output_stream_string",
  "get_pixel",
  "get_plot_option",
  "get_tex_environment",
  "get_tex_environment_default",
  "get_vertex_label",
  "getcurrentdirectory",
  "getenv",
  "gfactor",
  "gfactorsum",
  "ggf",
  "GGFCFMAX",
  "GGFINFINITY",
  "girth",
  "global_variances",
  "globalsolve",
  "gnuplot_close",
  "gnuplot_command",
  "gnuplot_curve_styles",
  "gnuplot_curve_titles",
  "gnuplot_default_term_command",
  "gnuplot_dumb_term_command",
  "gnuplot_file_args",
  "gnuplot_file_name",
  "gnuplot_out_file",
  "gnuplot_pdf_term_command",
  "gnuplot_pm3d",
  "gnuplot_png_term_command",
  "gnuplot_postamble",
  "gnuplot_preamble",
  "gnuplot_ps_term_command",
  "gnuplot_replot",
  "gnuplot_reset",
  "gnuplot_restart",
  "gnuplot_start",
  "gnuplot_svg_term_command",
  "gnuplot_term",
  "gnuplot_view_args",
  "go",
  "Gosper",
  "Gosper_in_Zeilberger",
  "GosperSum",
  "gr2d",
  "gr3d",
  "gradef",
  "gradefs",
  "gramschmidt",
  "graph6_decode",
  "graph6_encode",
  "graph6_export",
  "graph6_import",
  "graph_center",
  "graph_charpoly",
  "graph_eigenvalues",
  "graph_flow",
  "graph_order",
  "graph_periphery",
  "graph_product",
  "graph_size",
  "graph_union",
  "great_rhombicosidodecahedron_graph",
  "great_rhombicuboctahedron_graph",
  "grid",
  "grid",
  "grid2d",
  "grid_graph",
  "grind",
  "grobner_basis",
  "grotzch_graph",
  "halfangles",
  "hamilton_cycle",
  "hamilton_path",
  "hankel",
  "hankel_1",
  "hankel_2",
  "harmonic",
  "harmonic_mean",
  "hav",
  "head_angle",
  "head_angle",
  "head_both",
  "head_length",
  "head_length",
  "head_type",
  "heawood_graph",
  "height",
  "height",
  "hermite",
  "hessian",
  "hgfred",
  "hilbert_matrix",
  "hilbertmap",
  "hipow",
  "histogram",
  "histogram_description",
  "hodge",
  "horner",
  "hypergeometric",
  "hypergeometric_representation",
  "hypergeometric_simp",
  "ibase",
  "ic1",
  "ic2",
  "ic_convert",
  "icc1",
  "icc2",
  "ichr1",
  "ichr2",
  "icosahedron_graph",
  "icosidodecahedron_graph",
  "icounter",
  "icurvature",
  "ident",
  "identfor",
  "identity",
  "idiff",
  "idim",
  "idummy",
  "idummyx",
  "ieqn",
  "ieqnprint",
  "if",
  "ifactors",
  "ifb",
  "ifc1",
  "ifc2",
  "ifg",
  "ifgi",
  "ifr",
  "iframe_bracket_form",
  "iframes",
  "ifri",
  "ifs",
  "igcdex",
  "igeodesic_coords",
  "igeowedge_flag",
  "ikt1",
  "ikt2",
  "ilt",
  "image",
  "imaginary",
  "imagpart",
  "imetric",
  "imetric",
  "implicit",
  "implicit_derivative",
  "implicit_plot",
  "in",
  "in_neighbors",
  "inchar",
  "increasing",
  "ind",
  "indexed_tensor",
  "indices",
  "induced_subgraph",
  "inf",
  "inference_result",
  "inferencep",
  "infeval",
  "infinity",
  "infix",
  "inflag",
  "info_display",
  "infolists",
  "init_atensor",
  "init_ctensor",
  "inm",
  "inmc1",
  "inmc2",
  "innerproduct",
  "inpart",
  "inprod",
  "inrt",
  "intanalysis",
  "integer",
  "integer_partitions",
  "integerp",
  "integervalued",
  "integrate",
  "integrate_use_rootsof",
  "integration_constant",
  "integration_constant_counter",
  "interpolate_color",
  "intersect",
  "intersection",
  "intervalp",
  "intfaclim",
  "intopois",
  "intosum",
  "inv_mod",
  "invariant1",
  "invariant2",
  "inverse_fft",
  "inverse_jacobi_cd",
  "inverse_jacobi_cn",
  "inverse_jacobi_cs",
  "inverse_jacobi_dc",
  "inverse_jacobi_dn",
  "inverse_jacobi_ds",
  "inverse_jacobi_nc",
  "inverse_jacobi_nd",
  "inverse_jacobi_ns",
  "inverse_jacobi_sc",
  "inverse_jacobi_sd",
  "inverse_jacobi_sn",
  "inverse_real_fft",
  "invert",
  "invert_by_adjoint",
  "invert_by_lu",
  "ip_grid",
  "ip_grid_in",
  "irr",
  "irrational",
  "is",
  "is_biconnected",
  "is_bipartite",
  "is_connected",
  "is_digraph",
  "is_edge_in_graph",
  "is_graph",
  "is_graph_or_digraph",
  "is_isomorphic",
  "is_planar",
  "is_sconnected",
  "is_tree",
  "is_vertex_in_graph",
  "ishow",
  "isolate",
  "isolate_wrt_times",
  "isomorphism",
  "isqrt",
  "isreal_p",
  "items_inference",
  "iterations",
  "itr"]


 


