library (dplyr)
library (RODBC)

dbconn=odbcConnectAccess("data/laquina.mdb")

laq = sqlFetch(dbconn,"lqallith", stringsAsFactors = FALSE)

odbcClose(dbconn)

TypeList = readRDS('../piv analysis/data/piv-typelist.rds')

laq[laq==0] = NA

laq = laq %>%
  mutate(CODE = recode(CODE,
                       'F' = 'Flake',
                       'R' = 'Retouched',
                       'C' = 'Core',
                       'L' = 'Lithic other than flint',
                       'O' = 'Unworked',
                       'B' = 'Biface',
                       '4' = 'Natural')) %>%
  mutate(BRKDN = recode(BRKDN,
                        '1' = 'Scraper',
                        '2' = 'Notched',
                        '3' = 'Other retouched',
                        '4' = 'Broken scraper',
                        '5' = 'Broken notched',
                        '6' = 'Broken other retouched',
                        '7' = 'COMPFLAKE',
                        '8' = 'COMPFLAKE nearly',
                        '9' = 'PROXFLAKE', 
                        '10' = 'Split piece',
                        '11' = 'MEDFLAKE/DISTFLAKE',
                        '12' = 'Flat fragment',
                        '13' = 'Angular fragment',
                        '14' = 'Blocky fragment',
                        '15' = 'Chunk',
                        '16' = 'Core',
                        '17' = 'Flake core',
                        '18' = 'Other')) %>%
  mutate(MAT = recode(MAT,
                      '1' = 'Local Gray/Black Flint',
                      '2' = 'Yellow/Tan Flint',
                      '3' = 'Gray Striped Flint',
                      '4' = 'Jasper',
                      '5' = 'Chalcedony',
                      '6' = 'White/Tan Flint',
                      '7' = 'Quartz',
                      '8' = 'Silicified Limestone',
                      '9' = 'Exotic Flint',
                      '10' = 'Altered Material',
                      '11' = 'Limestone/Calcareous',
                      '12' = 'Limonite',
                      '13' = 'Basalt',
                      '14' = 'Other noncryptocrystalline')) %>%
  mutate(FLCND = recode(FLCND,
                        '1' = 'Complete',
                        '2' = 'Complete largely',
                        '3' = 'Proximal',
                        '4' = 'Medial',
                        '5' = 'Medial+Distal',
                        '6' = 'Medial',
                        '7' = 'Distal',
                        '8' = 'Right lateral',
                        '9' = 'Left lateral',
                        '10' = 'Other lateral',
                        '11' = 'Flat fragment',
                        '12' = 'Angular fragment',
                        '13' = 'Blocky fragment',
                        '14' = 'Irregular chunk')) %>%
  mutate(FCLAS = recode(FCLAS,
                        '1' = 'Normal',
                        '2' = 'Typical Levallois',
                        '3' = 'Atypical Levallois',
                        '4' = 'Levallois trimming flake',
                        '5' = 'First-order Levallois point',
                        '6' = 'Second-order Levallois point',
                        '7' = 'First-order prismatic blade',
                        '8' = 'Second-order prismatic blade',
                        '9' = 'Burin spall', 
                        '10' = 'Pseudo-Levallois point',
                        '11' = 'Curved ridged disc core product',
                        '12' = 'Ridged triangle',
                        '13' = 'Other disk core product',
                        '14' = 'Core-edge-backed knife',
                        '15' = 'Facet-backed knife',
                        '16' = 'Facet+cortex-backed knife',
                        '17' = 'Biface trimming flake',
                        '18' = 'Scraper sharpening flake',
                        '19' = 'Scraper edge Clactonian notch flake',
                        '20' = 'Lateral scraper edge removal flake',
                        '21' = 'Scraper end or tip removal',
                        '22' = 'Denticulate edge Clactonian notch flake', 
                        '23' = 'Laterial denticulate edge removal flake',
                        '24' = 'Other denticulate edge removal flake',
                        '25' = 'Notch renewal flake',
                        '26' = 'Clactonian notch flake',
                        '27' = 'Janus flake (full surface)',
                        '28' = 'Distal Janus flake',
                        '29' = 'Lateral Janus flake',
                        '30' = 'Flake with sharp Janus like edge',
                        '31' = 'Secondary multiple flake',
                        '32' = 'Clactonian flake',
                        '33' = 'Naturally backed knife',
                        '34' = 'Other patterned flake')) %>%
  mutate(PLCND = recode(PLCND,
                       '1' = 'Restricted',
                       '2' = 'Continuous down right margin',
                       '3' = 'Continuous down left margin',
                       '4' = 'Removed',
                       '5' = 'Crushed',
                       '6' = 'Missing',
                       '7' = '')) %>%
  mutate(PLCHR = recode(PLCHR,
                       '1' = 'Concave plain',
                       '2' = 'Concave dihedral', 
                       '3' = 'Concave facetted',
                       '4' = 'Flat plain',
                       '5' = 'Flat dihedral',
                       '6' = 'Flat faceted',
                       '7' = 'Slightly convex plain', 
                       '8' = 'Slightly convex dihedral',
                       '9' = 'Slightly convex faceted',
                       '10' = 'Strongly convex plain',
                       '11' = 'Strongly convex dihedral',
                       '12' = 'Stongly convex faceted',
                       '13' = 'Chapeau de Gendarme')) %>%
  mutate(PLSH = recode(PLSH,
                      '1' = 'Concave',
                      '2' = 'Flat',
                      '3' = 'Slightly convex',
                      '4' = 'Strongly convex')) %>%
  mutate(PLFAC = recode(PLFAC,
                       '1' = 'Plain',
                       '2' = 'Dihedral',
                       '3' = 'Faceted')) %>%
  mutate(EPMAR = recode(EPMAR,
                       '1' = 'None',
                       '2' = 'Trimming',
                       '3' = 'Scraper retouch',
                       '4' = 'Denticulate retouch',
                       '5' = 'A notch',
                       '6' = 'Crushed')) %>%
  mutate(CREXT = recode(CREXT,
                       '1' = '90-100%',
                       '2' = '75-89%',
                       '3' = '50-74%',
                       '4' = '25-49%',
                       '5' = '10-24%',
                       '6' = '0-9%',
                       '7' = '0%')) %>%
  mutate(PLCRT = recode(PLCRT,
                        '1' = 'Absent',
                        '2' = 'Partial',
                        '3' = 'Total')) %>%
  mutate(EXCRT = recode(EXCRT,
                        '1' = 'Absent',
                        '2' = 'Medial',
                        '3' = 'Lateral right',
                        '4' = 'Lateral left',
                        '5' = 'Only proximal',
                        '6' = 'Transverse cortical back',
                        '7' = 'Only distal',
                        '8' = 'Irregular',
                        '9' = 'Cortex on a fragment or chunk',
                        '10' = 'Total or near total')) %>%
  mutate(EXSUR = recode(EXSUR,
                        '1' = 'Plain (or single scar',
                        '2' = 'Low ridges',
                        '3' = 'Moderate ridges',
                        '4' = 'Steep ridges')) %>%
  mutate(RTHI = recode(RTHI,
                       '1' = 'Thin',
                       '2' = 'Moderate',
                       '3' = 'Thick')) %>%
  mutate(INSUR = recode(INSUR,
                        '1' = 'Protruding (convex)',
                        '2' = 'Slightly convex',
                        '3' = 'Flat',
                        '4' = 'Slightly concave',
                        '5' = 'Concave',
                        '6' = 'Markedly concave')) %>%
  mutate(BULB = recode(BULB,
                       '1' = 'Prominent',
                       '2' = 'Moderate',
                       '3' = "Flat")) %>%
  mutate(SYM = recode(SYM,
                      '1' = 'Symmetrical',
                      '2' = 'Asymmetrical to left',
                      '3' = 'Asymmetrical to right')) %>%
  mutate(PROP = recode(PROP,
                       '1' = 'Short and wide',
                       '2' = 'Equal',
                       '3' = 'Long and narrow')) %>%
  mutate(EDGSH = recode(EDGSH,
                        '1' = 'Straight',
                        '2' = 'Convex',
                        '3' = 'Concave',
                        '4' = 'Irregular')) %>%
  mutate(TERM = recode(TERM,
                       '1' = 'Flat/sharp',
                       '2' = 'Convex',
                       '3' = 'Rounded',
                       '4' = 'Hinged',
                       '5' = 'Shapped',
                       '6' = 'Crushed',
                       '7' = 'Retouched')) %>%
  mutate(OUTL = recode(OUTL,
                       '1' = 'Expanding (triangular)',
                       '2' = 'Expanding (fan shaped)',
                       '3' = 'Contracting',
                       '4' = 'Lenticular or oval',
                       '5' = 'Circular',
                       '6' = 'Curved (crescentic)',
                       '7' = 'Rectangular',
                       '8' = 'Irregular')) %>%
  mutate(OVRHG = recode(OVRHG,
                       '1' = 'Right lateral',
                       '2' = 'Left lateral', 
                       '3' = 'Distal',
                       '4' = 'Right and distal',
                       '5' = 'Left and distal',
                       '6' = 'Right left and distal')) %>%
  mutate(POSR = recode(POSR,
                      '1' = 'Exterior',
                      '2' = 'Interior',
                      '3' = 'Exterior and interior')) %>%
  mutate(CORES = recode(CORES,
                       '1' = 'Levallois',
                       '2' = 'Blade',
                       '3' = 'Disc',
                       '4' = 'Demi-disc',
                       '5' = 'Pyramidal',
                       '6' = 'Single platform',
                       '7' = 'Single surface bidirectional',
                       '8' = 'Single surface unidirectional',
                       '9' = 'Cubical',
                       '10' = 'Prismatic',
                       '11' = 'Globolar',
                       '12' = 'Chopper',
                       '13' = 'Chopper tool',
                       '14' = 'Formless',
                       '15' = 'Diverse',
                       '16' = 'Core on flake',
                       '17' = 'Flaked cobble',
                       '18' = 'Flaked and battered', 
                       '19' = 'Hammerstone',
                       '20' = 'Split cobble',
                       '21' = 'Broken',
                       '22' = 'Manuport',
                       '23' = 'Spheroid',
                       '24' = 'Other',
                       '25' = 'Biface',
                       '26' = 'Biface tip',
                       '27' = 'Biface midsection',
                       '28' = 'Biface edge',
                       '29' = 'Other biface fragment')) %>%
  mutate(FBType1 = TLTYPE) %>%
  mutate(TLTYPE_Text = recode(TLTYPE,
                        '64' = 'Scraper fragment',
                        '65' = 'Denticualte fragment',
                        '66' = 'Other fragment tool',
                        '96' = 'Heavily flaked utilized',
                        '97' = 'Lightly flaked utilized',
                        '98' = 'Possibly lightly flaked utilized',
                        '100' = '1-2 flat flakes removed from interior surface',
                        '101' = 'Platform and/or bulb removal',
                        '142' = 'Clactonian notch',
                        '143' = 'Microdenticulate',
                        '243' = 'Backed Denticulate',
                        '600' = 'Biface',
                        '700' = 'Spheriods',
                        .default = as.character(TLTYPE))) %>%
  mutate(Unit = SQUARE,
         Code = CODE,
         Layer = LEVEL,
         Divergence_angle = ANG,
         Length = LEN,
         Width = WID,
         Thick = THI,
         Platform_width = PWID,
         Platform_thick = PDEP,
         Weight = WGT,
         Flake_condition = FLCND,
         Flake_classification = FCLAS,
         Platform_condition = PLCND,
         Platform_character = PLCHR,
         Platform_shape = PLSH,
         Platform_faceting = PLFAC,
         Exterior_platform_margin = EPMAR,
         Special_platform_features = PLFEA,
         Cortex = CREXT,
         Cortex_character_position = CORTX,
         Platform_cortex = PLCRT,
         Exterior_cortex = EXCRT,
         Position_overshot_edges = OVRHG,
         Exterior_surface_relative_thickness = EXSRTH,
         Exterior_surface = EXSUR,
         Relative_thickness = RTHI,
         Interior_lateral_profile = INSUR,
         Bulb = BULB,
         Flake_symmetry = SYM,
         Flake_proportions = PROP,
         Edge_shape = EDGSH,
         Termination = TERM,
         Outline = OUTL,
         Tool_type = TLTYPE,
         Retouch_position = POSR,
         Core_type = CORES,
         Breakdown = BRKDN,
         Material = MAT)

laq$Retouch_intensity = NA
laq$Retouch_intensity[laq$FBType1>7 & laq$FBType1<30] = "Normal"
laq$Retouch_intensity[laq$FBType1>107 & laq$FBType1<130] = "Light"
laq$Retouch_intensity[laq$FBType1>307 & laq$FBType1<330] = "Demi-Quina"
laq$Retouch_intensity[laq$FBType1>407 & laq$FBType1<430] = "Quina"

laq$FBType2 = NA
laq$FBType2[laq$FBType1>207 & laq$FBType1<230] = 42
laq$FBType2[laq$FBType1>507 & laq$FBType1<530] = 43

laq$FBType1[which(laq$FBType1>107 & laq$FBType1<130)] = laq$FBType1[which(laq$FBType1>107 & laq$FBType1<130)] - 100
laq$FBType1[which(laq$FBType1>207 & laq$FBType1<230)] = laq$FBType1[which(laq$FBType1>207 & laq$FBType1<230)] - 200
laq$FBType1[which(laq$FBType1>307 & laq$FBType1<330)] = laq$FBType1[which(laq$FBType1>307 & laq$FBType1<330)] - 300
laq$FBType1[which(laq$FBType1>407 & laq$FBType1<430)] = laq$FBType1[which(laq$FBType1>407 & laq$FBType1<430)] - 400
laq$FBType1[which(laq$FBType1>507 & laq$FBType1<530)] = laq$FBType1[which(laq$FBType1>507 & laq$FBType1<530)] - 500

laq$FBType1[which(laq$FBType1==142)] = 42
laq$FBType1[which(laq$FBType1==143)] = 43
laq$FBType1[which(laq$FBType1==243)] = 43
laq$FBType1[which(laq$FBType1==64)] = 62
laq$FBType1[which(laq$FBType1==65)] = 43
laq$FBType1[which(laq$FBType1==66)] = 62
laq$FBType1[which(laq$FBType1==100)] = 64
laq$FBType1[which(laq$FBType1==600)] = 100
laq$FBType1[which(laq$FBType1==700)] = 62

laq$Superimposed_bulb = ifelse(laq$Special_platform_features %in% c(1,4,7,10), TRUE, NA)
laq$Marked_medial_exterior_projection = ifelse(laq$Special_platform_features %in% c(2,5,8,11), TRUE, NA)
laq$Interior_clactonian_angle = ifelse(laq$Special_platform_features %in% c(3,4,5,9,10,11), TRUE, NA)
laq$Interior_lip = ifelse(laq$Special_platform_features %in% c(6,7,8,9,10,11), TRUE, NA)

laq = laq %>%
  select(Unit, ID, Layer, X, Y, Z, Code, Breakdown, Material, 
       Flake_condition,
       Flake_classification,
       Platform_condition,
       Platform_character,
       Platform_shape,
       Platform_faceting,
       Exterior_platform_margin,
       Superimposed_bulb,
       Marked_medial_exterior_projection,
       Interior_clactonian_angle,
       Interior_lip,
       Cortex,
       Cortex_character_position,
       Platform_cortex,
       Exterior_cortex,
       Position_overshot_edges,
       Exterior_surface_relative_thickness,
       Exterior_surface,
       Relative_thickness,
       Interior_lateral_profile,
       Bulb,
       Flake_symmetry,
       Flake_proportions,
       Edge_shape,
       Termination,
       Outline,
       Tool_type,
       FBType1,
       FBType2,
       Retouch_intensity,
       Retouch_position,
       Core_type,
       Divergence_angle,
       Length,
       Width,
       Thick,
       Platform_width,
       Platform_thick,
       Weight)

saveRDS(laq, 'data/laq jelinek.RDS')
write.csv(laq, 'data/laq jelinek.csv')

laq$Dataclass = laq$Breakdown
laq$Dataclass[laq$Dataclass == 'Scraper'] = 'COMPTOOL'
laq$Dataclass[laq$Dataclass == 'Notched'] = 'COMPTOOL'
laq$Dataclass[laq$Dataclass == 'Other retouched'] = 'COMPTOOL'
laq$Dataclass[laq$Dataclass == 'Broken notched'] = 'DISTTOOL'
laq$Dataclass[laq$Dataclass == 'Broken other retouched'] = 'DISTTOOL'
laq$Dataclass[laq$Dataclass == 'Broken scraper'] = 'DISTTOOL'
laq$Dataclass[laq$Dataclass == 'MEDFLAKE/DISTFLAKE'] = 'DISTFLAKE'
laq$Dataclass[laq$Dataclass == 'Angular fragment'] = 'SHATTER'
laq$Dataclass[laq$Dataclass == 'Blocky fragment'] = 'SHATTER'
laq$Dataclass[laq$Dataclass == 'Chunk'] = 'SHATTER'
laq$Dataclass[laq$Dataclass == 'Core'] = 'CORE'
laq$Dataclass[laq$Dataclass == 'Flake core'] = 'CORE'
laq$Dataclass[laq$Dataclass == 'Flat fragment'] = 'SHATTER'
laq$Dataclass[laq$Dataclass == 'Other'] = 'OTHER'
laq$Dataclass[laq$Dataclass == 'Split piece'] = 'DISTFLAKE'
laq$Dataclass[laq$Dataclass == 'COMPFLAKE nearly'] = 'COMPFLAKE'

laq$Dataclass[laq$Core_type=='Biface'] == 'BIFACE'
laq$Dataclass[laq$Core_type=='Biface tip'] == 'BIFFRAG'
laq$Dataclass[laq$Core_type=='Biface edge'] == 'BIFFRAG'
laq$Dataclass[laq$Core_type=='Biface midsection'] == 'BIFFRAG'
laq$Dataclass[laq$Core_type=='Other biface fragment'] == 'BIFFRAG'

laq$Dataclass[laq$Flake_condition == 'Medial' & laq$Dataclass == 'DISTFLAKE'] = 'MEDFLAKE'
laq$Dataclass[laq$Flake_condition == 'Medial' & laq$Dataclass == 'DISTFLAKE'] = 'MEDFLAKE'

laq$Dataclass[laq$Flake_condition == 'Proximal' & laq$Dataclass == 'DISTTOOL'] = 'PROXTOOL'
laq$Dataclass[laq$Flake_condition == 'Medial' & laq$Dataclass == 'DISTTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Flake fragment' & laq$Dataclass == 'DISTTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Angular fragment' & laq$Dataclass == 'DISTTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Blocky fragment' & laq$Dataclass == 'DISTTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Complete largely' & laq$Dataclass == 'DISTTOOL'] = 'COMPTOOL'

laq$Dataclass[laq$Flake_condition == 'Proximal' & laq$Dataclass == 'COMPTOOL'] = 'PROXTOOL'
laq$Dataclass[laq$Flake_condition == 'Medial' & laq$Dataclass == 'COMPTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Distal' & laq$Dataclass == 'COMPTOOL'] = 'DISTTOOL'
laq$Dataclass[laq$Flake_condition == 'Medial+Distal' & laq$Dataclass == 'COMPTOOL'] = 'DISTTOOL'
laq$Dataclass[laq$Flake_condition == 'Angular fragment' & laq$Dataclass == 'COMPTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Blocky fragment' & laq$Dataclass == 'COMPTOOL'] = 'MEDTOOL'
laq$Dataclass[laq$Flake_condition == 'Flat fragment' & laq$Dataclass == 'COMPTOOL'] = 'MEDTOOL'

laq$Scraper = FALSE
laq$Scraper[which(laq$FBType1>=6 & laq$FBType1<=29)] = TRUE
laq$Notch = FALSE
laq$Notch[which(laq$FBType1 %in% c(42,43,51,54))] = TRUE

real_types = c(1:45,48,50:65)
essential_types = c(4,6:37,39:45,50:64,100)

laq$Tool = FALSE
laq$Tool[which(laq$FBType1 %in% essential_types)] = TRUE

laq$Flake = FALSE
laq$Flake[which(laq$Dataclass %in% c('COMPFLAKE','DISTFLAKE','MEDFLAKE','PROXFLAKE'))] = TRUE

laq$Platform_flake = FALSE
laq$Platform_flake[which(laq$Dataclass %in% c('COMPFLAKE','PROXFLAKE'))] = TRUE

laq$Platform_tool = FALSE
laq$Platform_tool[which(laq$Dataclass %in% c('COMPTOOL','PROXTOOL'))] = TRUE

laq$Technique = NA
laq$Technique[laq$FBTYPE1 %in% c(1,2,3,4)] = "LEVAL"
laq$Technique[laq$Flake_classification %in% c('Typical Levallois',
                                              'Atypical Levallois',
                                              'Levallois trimming flake',
                                              'First-order Levallois point',
                                              'Second-order Levallois point')] = "LEVAL"
laq$Technique[laq$Flake_classification=='Burin spall'] = "BURIN"
laq$Technique[laq$Flake_classification %in% c('Normal',
                                              'Naturally backed knife',
                                              'Core-edge-backed knife')] = "NORMAL"
laq$Technique[laq$Flake_classification %in% c('First-order prismatic blade',
                                              'Second-order prismatic blade')] = "BLADE"
laq$Technique[laq$Flake_classification=='Pseudo-Levallois'] = "DISC"
laq$Technique[laq$Flake_classification=='Biface trimming flake'] = "BIFRET"
laq$Technique[laq$Flake_classification=='Scraper resharpening flake'] = "RETFLK"
laq$Technique[laq$Flake_classification=='Scraper resharpening flake'] = "RETFLK"
laq$Technique[laq$Flake_classification=='Scraper edge Clactonian notch flake'] = "NOTCH"
laq$Technique[laq$Flake_classification=='Denticulate edge Clactonian notch flake'] = "NOTCH"
laq$Technique[laq$Flake_classification=='Clactonian notch flake'] = "NOTCH"
laq$Technique[laq$Flake_classification=='Notch renewal flake'] = "NOTCH"
laq$Technique[laq$Flake_classification %in% c('Janus flake (full surface)',
                                              'Distal Janus flake',
                                              'Lateral Janus flake',
                                              'Flake with sharp Janus like edge')] = "KOMBEWA"
laq$Technique[laq$Flake_classification %in% c('Lateral scraper edge removal flake',
                                              'Lateral denticulate edge removal flake',
                                              'Scraper end or tip removal')] = "TRANCHET"
laq$Technique[laq$Flake_classification %in% c('Other denticulate edge removal flake',
                                              'Other patterned flake',
                                              'Curved ridged disc core product',
                                              'Ridged triangle',
                                              'Facet-backed knife',
                                              'Facet+cortex-backed knife',
                                              'Other disk core product')] = "OTHER"
laq$Technique[laq$Flake_classification=='Clactonian flake'] = "CLACTON"

laq$Form = NA
laq$Form[laq$Breakdown == 'Angular fragment'] = 'ANGULAR'
laq$Form[laq$Breakdown == 'Chunk'] = 'CHUNK'
laq$Form[laq$Breakdown == 'Core'] = 'CORE'
laq$Form[laq$Dataclass == 'BIFACE' | laq$Dataclass == 'BIFFRAG'] = 'BIFACE'
laq$Form[laq$Flake_classification=='Naturally backed knife'] = "NATBACK"
laq$Form[laq$Flake_classification=='Core-edge-backed knife'] = "DEBORD"
laq$Form[laq$Position_overshot_edges %in% c('Distal',
                                            'Left and distal',
                                            'Right and distal',
                                            'Right left and distal')] = 'OVERSHOT'
laq$Form[laq$Core_type == 'Biface edge'] = 'BIFEDGE'
laq$Form[laq$Flake_classification %in% c('Lateral denticulate edge removal flake',
                                         'Lateral scraper edge removal flake')] = "LSF"
laq$Form[laq$Flake_classification == 'Scraper end or tip removal'] = 'TRANCHET'
laq$Form[laq$Flake_classification %in% c('First-order Levallois point',
                                         'Second-order Levallois point')] = "POINT"
laq$Form[laq$Flake_classification %in% c('First-order prismatic blade',
                                              'Second-order prismatic blade')] = "BLADE"
laq$Form[laq$Outline %in% c('Expanding (triangular)',
                            'Expanding (fan shaped)')] = 'EXPANDING'
laq$Form[laq$Dataclass == 'Shatter'] = 'SHATTER'

laq = laq[,c("Unit","ID",
             "Layer","X","Y","Z",
             "Material",
             "Breakdown","Flake_condition","Flake_classification",
             "Dataclass","Technique","Form","Platform_condition",
             "Platform_character","Platform_shape",           
             "Platform_faceting","Exterior_platform_margin",
             "Platform_width","Platform_thick",
             "Superimposed_bulb","Marked_medial_exterior_projection",
             "Interior_clactonian_angle","Interior_lip",
             "Cortex","Platform_cortex","Exterior_cortex",
             "Position_overshot_edges",
             "Exterior_surface","Relative_thickness",
             "Interior_lateral_profile","Bulb",
             "Flake_symmetry","Flake_proportions",
             "Edge_shape","Termination","Outline",
             "Tool_type","FBType1","FBType2",
             "Retouch_position","Retouch_intensity",
             "Core_type",
             "Divergence_angle",
             "Length","Width","Thick","Weight",
             "Scraper","Notch","Tool","Flake",
             "Platform_flake","Platform_tool")]

saveRDS(laq, 'data/laq lithics.RDS')
write.csv(laq, 'data/laq lithics.csv', row.names = FALSE)
