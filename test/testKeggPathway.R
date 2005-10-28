library("pathRender")

testKeggPathway <- function() {
  pkname = c(
        "hsa00340", "hsa00350", "hsa00240", "hsa00640", "hsa00780",
        "hsa00052", "hsa00272", "hsa00960", "hsa00190", "hsa00790",
        "hsa00140", "hsa00480", "hsa00590", "map00730", "hsa00280",
        "hsa00860", "hsa00601", "hsa00620", "hsa00230", "hsa00062",
        "hsa00330", "hsa00380", "hsa00071", "hsa00940", "hsa00520",
        "hsa00920", "hsa00300", "hsa00670", "hsa00532", "hsa00440",
        "hsa00650", "hsa00072", "hsa00460", "hsa00602", "hsa00130",
        "hsa00562", "hsa00970", "hsa00604", "hsa00600", "hsa00680",
        "hsa00830", "hsa00500", "hsa00220", "hsa00910", "hsa00630",
        "hsa00580", "hsa00740", "hsa00770", "hsa00750", "hsa00020",
        "hsa00150", "hsa00010", "hsa00310", "map00473", "hsa00271",
        "hsa00290", "map00540", "hsa00900", "hsa00450", "hsa00120",
        "hsa00603", "hsa00760", "hsa00530", "hsa00260", "hsa00570",
        "hsa00950", "hsa00251", "hsa00471", "hsa00510", "hsa00100",
        "hsa00252", "hsa00051", "map00901", "hsa00400", "hsa00030",
        "hsa00561", "hsa00061", "hsa00512", "hsa00430", "hsa00053",
        "hsa00031", "hsa00360", "hsa00410", "hsa00472", "hsa00040"
           )

  for ( i in 1:length(pkname) ) {
      print(i)
      rendercMAPPathway(pname=pkname[i]);
  }
}

