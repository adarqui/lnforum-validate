{-# LANGUAGE OverloadedStrings #-}

module LN.Generate.Default.Leuron (
  defaultFact,
  defaultFactList,
  defaultCard,
  defaultDCard,
  defaultDCardX,
  defaultAcronym,
  defaultSynonym,
  defaultAntonym,
  defaultTemplate,
  defaultImageAssociation,
  defaultLinearDemo,
  defaultTable,
  defaultScript,
  defaultQA
) where



import           LN.T



defaultFact :: Fact
defaultFact = Fact ""



defaultFactList :: FactList
defaultFactList = FactList "" []



defaultCard :: Card
defaultCard = Card "" ""



defaultDCard :: DCard
defaultDCard = DCard "" ""



defaultDCardX :: DCardX
defaultDCardX = DCardX [] []



defaultAcronym :: Acronym
defaultAcronym = Acronym "" ""



defaultSynonym :: Synonym
defaultSynonym = Synonym "" ""



defaultAntonym :: Antonym
defaultAntonym = Antonym "" ""



defaultTemplate :: Template
defaultTemplate = Template "" []



defaultImageAssociation :: ImageAssociation
defaultImageAssociation = ImageAssociation [] [] []



defaultLinearDemo :: LinearDemo
defaultLinearDemo = LinearDemo "" []



defaultTable :: Table
defaultTable = Table "" [] []



defaultScript :: Script
defaultScript = Script "" "" ""



defaultQA :: QA
defaultQA = QA "" ""
