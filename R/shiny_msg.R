lan <- "en"

.msg <- list(
  deleteConfirm = c(
    cn = "您的参数可能已经更新。如果保存目前的参数，之前计算的结果将会被删除！
    您确定要删除之前的计算结果并进入下一步吗？当然您可以继续后面的步骤并重新计算。",
    en = "Parameters updated. If you save the current parameter and continue, 
    the calculated results will be deleted. Do you want to delete the old results
    and continue?"
    ),
  delandgo = c(
    cn = "删除并进入下一步",
    en = "Delete and continue"
    ),
  loading = c(
    cn = "载入数据",
    en = "Input"
    ),
  loading2 = c(
    cn = "数据加载中...",
    en = "Loading data ..."
    ),
  expdesign = c(
    cn = "实验设计",
    en = "Experimental design"
    ),
  editexpdesign = c(
    cn = "编辑实验样本信息",
    en = "Editting experimental design"
    ),
  savetemplate = c(
    cn = "保存模板",
    en = "Save template"
    ),
  uploadtemplate = c(
    cn = "上传模板",
    en = "Uploading template"
    ),
  delsample = c(
    cn = "去除样本",
    en = "Remove sample"
    ),
  seldelsample = c(
    cn = "请选择想要去除的样本",
    en = "Please select samples to be removed"
    ),
  sampleinfo = c(
    cn = "样本信息展示",
    en = "Sample information"
    ),
  featureinfo = c(
    cn = "基因/蛋白信息",
    en = "Feature information"
    ),
  calculating = c(
    cn = "努力计算中...",
    en = "Calculating ...."
    ),
  toomanycolor = c(
    cn = "您选择的变量不适合匹配不同颜色：太多不同的值!",
    en = "The selected variable cannot be distinguished by color: too many unique values. "
    ),
  onemorecheck = c(
    cn = "请再检查一下",
    en = "Please check"
    ),
  checknorm = c(
    cn = "亲，请先查看一种规范化方法再保存设置!",
    en = "Please check one normalization method before saving setting!"
    ),
  ok = c(
    cn = "好吧",
    en = "OK"
    ),
  filter = c(
    cn = "筛除蛋白",
    en = "Filtering"
    ),
  datanorm = c(
    cn = "数据规范化",
    en = "Normalization"
    ),
  funannot = c(
    cn = "功能注释",
    en = "Annotation"),
  otherset = c(
    cn = "其他设置",
    en = "Other settings"
    ),
  reviewset = c(
    cn = "查看设置",
    en = "Review settings"
    ),
  stats = c(
    cn = "统计分析",
    en = "Statistics"
    ),
  ttest = c(
    cn = "两组比较 t-检验",
    en = "t-test"
    ),
  cortest = c(
    cn = "相关性分析",
    en = "Correlation"
    ),
  outliertest = c(
    cn = "离群蛋白分析",
    en = "Outlier"
    ),
  stepupdown = c(
    cn = "表达递增/递减",
    en = "Steps"
    ),
  checkres = c(
    cn = "查看结果",
    en = "Check result"
    ),
  waitset = c(
    cn = "待设置",
    en = "Pending"
    ),
  notanalysed = c(
    cn = "未分析",
    en = "Not done"),
  nores = c(
    cn = "暂无结果",
    en = "No result"
    ),
  ready = c(
    cn = "可查看",
    en = "Ready"),
  problem = c(
    cn = "可能有点问题 ...", 
    en = "Perhaps there is a problem ..."
    ),
  paramset = c(
    cn = "参数设置",
    en = "Setting"
    ),
  helpmanu = c(
    cn = "帮助文档",
    en = "Doc"),
  saved = c(
    cn = "已保存",
    en = "Saved"),
  skip = c(
    cn = "跳过",
    en = "Skip"),
  saveset = c(
    cn = "保存设置", 
    en = "Save"),
  filterfeat = c(
    cn = "筛除低质量基因/蛋白",
    en = "Filter out features"),
  filter1 = c(
    cn = "根据蛋白的最高表达量筛除",
    en = "According to the maximum expression of features"
    ),
  filterprot = c(
    cn = "筛除的蛋白/基因",
    en = "Excluded features"
    ),
  keepprot = c(
    cn = "保留的蛋白/基因",
    en = "Included features"),
  fl1 = c(
    cn = "蛋白的最高表达量不应该低于:", 
    en = "The feature's max expression should be higher than:"
    ),
  fl1desc = c(
    cn = "输入某个分位数，应介于0和1之间！",
    en = "Give a quantile between 0 and 1!"
    ),
  filter2 = c(
    cn = "根据缺失值筛除（选择一个变量，由此变量定义的组别中至少有N个值）",
    en = "According to missing values "
    ), 
  choosevar = c(
    cn = "选择变量",
    en = "Select a variable"
    ),
  fvardesc = c(
    cn = "在选择的变量中，至少某一组有N个非缺失值",
    en = "According to the selected variables, 
    there should be at list one group contain at least N values"
    ),
  valnotsame = c(
    cn = "两组的值不能够相同！", 
    en = "The two values need to be different!"
    ),
  somethingwrong = c(
    cn = "有点问题...",
    en = "Something wrong ..."
    ),
  ttestexist = c(
    cn = "这组比较已经加进去了！", 
    en = "This group has been added!"
    ),
  ttestmax = c(
    cn = "最多允许32组两两比较，太多也看不过来，要不想想其他的分析方法吧。",
    en = "Maximum 32 comparisons allow!"
    ),
  ttesttitle = c(
    cn = "两组比较 t-检验",
    en = "Two groups differential expression - t-test"
    ),
  analyzecheck = c(
    cn = "分析并查看结果",
    en = "Get result"
    ),
  ttest_selectvar = c(
    cn = "请选择样本信息变量",
    en = "Select variable for comparison"
    ),
  add = c(
    cn = "添加", 
    en = "Add"
    ),
  ttest_addall = c(
    cn = "添加所有两两比较", 
    en = "Add all pairs"
    ),
  ttestvar1 = c(
    cn = "选择第一组样本",
    en = "Select group 1"
    ),
  ttestvar2 = c(
    cn = "选择第二组样本",
    en = "Select group 2"
    ),
  ttestadded = c(
    cn = "已经加入的两两比较",
    en = "Added comparisons"
    ), 
  ttest_deleteall = c(
    cn = "删除所有比较", 
    en = "Delete all comparisons"
    ),
  ttest_deleteselected = c(
    cn = "删除选中的比较",
    en = "Delete selected comparison"
    ),
  annot_title = c(
    cn = "基因/蛋白注释",
    en = "Feature functional annotation"
    ),
  annot_database = c(
    cn = "注释使用的数据库", 
    en = "Annotation database"
    ),
  annot_databaseselect = c(
    cn = "选择注释数据",
    en = "Select database"
    ),
  annot_databasecol = c(
    cn = "用于注释的列",
    en = "ID column mapped to data"
    ),
  annot_show = c(
    cn = "功能注释数据库展示:",
    en = "Database example"
    ),
  annot_source = c(
    cn = "选择注释信息来源（source）:", 
    en = "Select sources of annotations"
    ),
  annot_source2 = c(
    cn = "Annotation sources", 
    en = "Annotation sources"),
  annot_datacol = c(
    cn = "选择用于注释的列：", 
    en = "ID column mapped to annotation"
    ),
  annot_subsep = c(
    cn = "区分亚型的字段",
    en = "Secondary separator (e.g. isoform)"
    ),
  cort_title = c(
    cn = "基因/蛋白相与样本变量的关性分析", 
    en = 'Correlation between feature and variables'
    ),
  cort_var = c(
    cn = "请选择样本变量进行相关性分析",
    en = "Select variables for correlation analysis"
    ),
  cort_holder = c(
    cn = "使用正则表达一次添加多个变量，例如 '^IC50'.",
    en = "Using regular expression to add multiple values, e.g. '^IC50'."
    ),
  cort_max = c(
    cn = "最多允许加入%s个变量进行相关性分析",
    cn = "Maximum %s variables allowed!"
    ),
  step_less2 = c(
    cn = "排序后的分组应该包含至少三个分组，请使用“t-检验”进行两组间比较!",
    en = "At least three values need for the step analysis, please used t-test for two groups comparison."
    ),
  step_max = c(
    cn = "最多允许12组的递增或递减分析！",
    en = "Maximum 12 analyses allowed!"
    ),
  step_title = c(
    cn = "找出表达递增或递减的基因或蛋白", 
    en = "Identify features step up/down"
    ),
  step_added = c(
    cn = "已添加的递增递减分析", 
    en = "Added analyses"),
  step_allgroup = c(
    cn = "所有分组",
    en = "All groups"
    ),
  step_order = c(
    cn = "分组排序", 
    en = "Order groups"
    ),
  outlier_title = c(
    cn = "离群蛋白分析", 
    en = "Outlier analysis"
    ),
  other_title = c(
    cn = "其他分析", 
    en = "Other analysis"
    ),
  other_finish = c(
    cn = "完成设置",
    en = "Finish"
    ),
  other_set = c(
    cn = "设置其他分析",
    en = "Other analysis"
    ),
  other_pca = c(
    cn = "进行主元分析 (PCA)",
    en = "Perform PCA"
    ),
  other_string = c(
    cn = "可以用于连接 STRING 数据库的列（一般是基因名或者 UniProt ID）", 
    en = "ID column for STRING analysis, e.g. gene name or UniProt ID"
    ),
  other_seqlogo = c(
    cn = "seqLogo 分析的列（一般是 Sequence window）",
    en = "Columns for seqLogo analysis, e.g. Sequence window"
    ),
  review_title = c(
    cn = "检查参数设置",
    en = "Review settings"
    ),
  review_confirm = c(
    cn = "完成数据预处理与注释",
    en = "Run analyses"
    ),
  review_methods = c(
    cn = "方法 (Methods) 部分写作参考例句",
    en = "Methods"
    ),
  norm_col = c(
    cn = "列(column-wise)规范化方法",
    en = "Column-wise normalization"
    ),
  norm_row = c(
    cn = "行(row-wise)规范化方法", 
    en = "Row-wise normalization"
    ),
  norm_export = c(
    cn = "导出规范化的数据", 
    en = "Export normalized data"
    ),
  norm_checkrow = c(
    cn = "蛋白信息（单击某一行选择并查看蛋白）",
    en = "Check individual feature"
    ),
  norm_colorvar = c(
    cn = "选择区分样本信息的变量",
    en = "Select variable of interest"
    ),
  norm_sampleinfo = c(
    cn = "样本提示信息", 
    en = "Sample labels"
    ),
  norm_comp1 = c(
    cn = "x轴的主成分",
    en = "Component on x-axis"
    ),
  norm_comp2 = c(
    cn = "y轴的主成分", 
    en = "Component on y-axis"
    ),
  norm_l1 = c(
    cn = "主成分分析（无插补数据：去除含缺失值的基因或蛋白）",
    en = "PCA (features with missing values excluded)"
    ),
  norm_l2 = c(
    cn = "主成分分析（插补后数据）",
    en = "PCA (missing values are filled by a constant)"
    ),
  norm_l3 = c(
    cn = "表达强度分布", 
    en = "Expression distribution"
    ),
  norm_l4 = c(
    cn = "鉴定到的基因/蛋白数目",
    en = "Feature ID"
    ),
  pending = c(
    cn = "(待计算）",
    en = "(Pending)"
    ),
  projectname = c(
    cn = "项目名称", 
    en = "Project name"
    ), 
  pcacheck = c(
    cn = "主元分析(PCA)",
    en = "Principal component analysis (PCA)"
    ),
  annot_nomatch = c(
    cn = "提供的基因或蛋白名称在选择的注释数据库中无法找到，这个问题通常是因为注释数据库中用于注释的列没有选对，或者基因/蛋白信息用于注释的列没有选对。",
    en = "The provided feature ID does not match any ID in the annotation database. Please check ID columns and make sure you selected correct database."
    ),
  sum_quantile = c(
    cn = "蛋白的最高表达值应高于该分位数: %s",
    en = "The feature's intensity should be higher than quantile: %s"
    ),
  sum_groupvar = c(
    cn = "所有样本可由此变量分组: %s",
    en = "Samples are divided into groups according variable: %s"
    ),
  sum_groupmin = c(
    cn = "至少有一个组中至少有 N 个值，N = %s",
    en = "Should have at least N non-missing values, N = %s"
    ),
  sum_retainedfeature = c(
    cn = "保留 %s 个蛋白，筛除 %s 个蛋白!",
    en = "%s features included, %s features excluded!"
    ),
  sum_motifcol = c(
    cn = "用于转录后修饰 Motif 分析的列",
    en = "Columns for Motif analysis"
    ),
  sum_coldatabase = c(
    cn = "用于查询基因功能注释数据库的列",
    en = "ID column in database for functional annotation"
    ),
  sum_coldata = c(
    cn = "用于查询基因功能注释数据库的蛋白信息列",
    en = "ID column in omics data for functional annotation"
    )
)

