library("pathRender")

testBioPathway <- function() {
   pbname = c(
        "aifpathway", "ppargpathway", "hoppathway", "p53pathway", "hbxpathway",
        "myosinpathway", "keratinocytepathway", "nthipathway", "cdc25pathway", "etspathway",
        "tubbypathway", "calcineurinpathway", "actinypathway", "d4gdipathway", "ctbp1pathway",
        "erbb3pathway", "plateletapppathway", "no2il12pathway", "rarpathway", "il22bppathway",
        "proteasomepathway", "arappathway", "pcafpathway", "at1rpathway", "tnfr1pathway",
        "ghpathway", "sam68pathway", "telpathway", "bard1pathway", "fibrinolysispathway",
        "her2pathway", "ecmpathway", "carmerpathway", "sppapathway", "tffpathway",
        "gata3pathway", "pparapathway", "ceramidepathway", "crebpathway", "mta3pathway",
        "cpsfpathway", "smpathway", "crempathway", "arfpathway", "eicosanoidpathway",
        "p53hypoxiapathway", "lis1pathway", "eponfkbpathway", "mhcpathway", "rnapol3pathway",
        "igf1pathway", "integrinpathway", "akap13pathway", "vifpathway", "reckpathway",
        "ps1pathway", "notchpathway", "stathminpathway", "il4pathway", "nfatpathway",
        "tpopathway", "pgc1apathway", "alternativepathway", "dbpbpathway", "par1pathway",
        "gleevecpathway", "edg1pathway", "gabapathway", "badpathway", "tcrapathway",
        "metpathway", "agpcrpathway", "flumazenilpathway", "npcpathway", "cacampathway",
        "tall1pathway", "il2rbpathway", "hivnefpathway", "ifngpathway", "chrebppathway",
        "fbw7pathway", "raccpathway", "mprpathway", "dnafragmentpathway", "cellcyclepathway",
        "cd40pathway", "ptenpathway", "pcsynthesispathway", "vegfpathway", "hsp27pathway",
        "ctcfpathway", "il2pathway", "ndkdynaminpathway", "ranpathway", "anthraxpathway",
        "biopeptidespathway", "achpathway", "ngfpathway", "ahsppathway", "rhopathway",
        "carm1pathway", "eif4pathway", "akapcentrosomepathway", "tgfbpathway", "nfkbpathway",
        "gspathway", "gcrpathway", "deathpathway", "mrppathway", "erbb4pathway",
        "skp2e2fpathway", "pkcpathway", "cb1rpathway", "no1pathway", "p38mapkpathway",
        "epopathway", "ccr5pathway", "igf1rpathway", "faspathway", "cell2cellpathway",
        "soddpathway", "ahrpathway", "fmlppathway", "trkapathway", "intrinsicpathway",
        "dicerpathway", "il6pathway", "bcrpathway", "tcrpathway", "rnapathway",
        "mapkpathway", "agrpathway", "rbpathway", "pmlpathway", "hifpathway",
        "leptinpathway", "lectinpathway", "wntlrp6pathway", "mcalpainpathway", "plcpathway",
        "sumopathway", "gsk3pathway", "fcer1pathway", "bcellsurvivalpathway", "btg2pathway",
        "ptc1pathway", "vdrpathway", "efppathway", "chemicalpathway", "pyk2pathway",
        "rhodopsinpathway", "nkcellspathway", "dreampathway", "g2pathway", "ctla4pathway",
        "vobesitypathway", "eifpathway", "aktpathway", "hswisnfpathway", "npp1pathway",
        "antisensepathway", "pdgfpathway", "cdmacpathway", "mcmpathway", "raccycdpathway",
        "s1ppathway", "tidpathway", "eif2pathway", "il3pathway", "arenrf2pathway",
        "ranmspathway", "fosbpathway", "il10pathway", "cblpathway", "pitx2pathway",
        "wntpathway", "barrestinsrcpathway", "cskpathway", "atmpathway", "prionpathway",
        "ucalpainpathway", "ranbp2pathway", "ifnapathway", "eradpathway", "p35alzheimerspathway",
        "circadianpathway", "malpathway", "apppathway", "acetaminophenpathway", "tnfr2pathway",
        "stat3pathway", "erk5pathway", "setpathway", "mitrpathway", "tsp1pathway",
        "atrbrcapathway", "ionpathway", "prc2pathway", "cdc42racpathway", "parkinsonspathway",
        "mtorpathway", "plk3pathway", "raspathway", "longevitypathway", "tercpathway",
        "cdk5pathway", "cardiacegfpathway", "pelp1pathway", "egfpathway", "hespathway",
        "plcepathway", "relapathway", "il1rpathway", "il7pathway", "plcdpathway",
        "barrestinpathway", "il12pathway", "salmonellapathway", "caspasepathway", "reelinpathway",
        "stresspathway", "mef2dpathway", "wnvpathway", "dsppathway", "p27pathway",
        "ck1pathway", "insulinpathway", "parkinpathway", "cftrpathway", "classicpathway",
        "gpcrpathway", "smrtepathway", "erkpathway", "41bbpathway", "mbdpathway",
        "alkpathway", "rarrxrpathway", "sprypathway", "nos1pathway", "pparpathway",
        "ptdinspathway", "huntingtonpathway", "g1pathway", "mitochondriapathway", "barrmapkpathway",
        "ccr3pathway", "igf1mtorpathway", "botulinpathway", "rac1pathway", "irespathway",
        "tertpathway", "hdacpathway", "ranklpathway", "extrinsicpathway", "akap95pathway",
        "hcmvpathway", "melanocytepathway", "cxcr4pathway", "tollpathway"
   )

   for ( i in 1:length(pbname) ) {
         print(i)
         rendercMAPPathway(pname=pbname[i])
   }
}

