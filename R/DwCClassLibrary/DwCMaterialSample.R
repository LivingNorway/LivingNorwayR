# A physical result of a sampling (or subsampling) event. In biological collections, the material sample is typically collected, and either preserved or destructively processed (http://rs.tdwg.org/dwc/terms/MaterialSample)
setRefClass(
  Class = "DwCMaterialSample",
  fields = list(
    # An identifier for the MaterialSample (as opposed to a particular digital record of the material sample). In the absence of a persistent global unique identifier, construct one from a combination of identifiers in the record that will most closely make the materialSampleID globally unique (http://rs.tdwg.org/dwc/terms/materialSampleID)
    materialSampleID = "character"
  ),
  contains = "DwCComponent"
)