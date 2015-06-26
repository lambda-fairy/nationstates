-- | Data structures used by NationStates.

module NationStates.Types where


-- | Nation category.
--
-- This category summarizes a nation's personal, economic, and political
-- freedoms.
data WACategory
    = Anarchy
    | AuthoritarianDemocracy
    | BenevolentDictatorship
    | CapitalistParadise
    | Capitalizt
    | CivilRightsLovefest
    | CompulsoryConsumeristState
    | ConservativeDemocracy
    | CorporateBordello
    | CorporatePoliceState
    | CorruptDictatorship
    | DemocraticSocialists
    | FatherKnowsBestState FatherOrMother
        -- ^ This category has two variations: \"/Father/ Knows Best
        -- State\" and \"/Mother/ Knows Best State\".
    | FreeMarketParadise
    | InoffensiveCentristDemocracy
    | IronFistConsumerists
    | IronFistSocialists
    | LeftLeaningCollegeState
    | LeftWingUtopia
    | LiberalDemocraticSocialists
    | LibertarianPoliceState
    | MoralisticDemocracy
    | NewYorkTimesDemocracy
    | PsychoticDictatorship
    | RightWingUtopia
    | ScandinavianLiberalParadise
    | TyrannyByMajority
    deriving (Eq, Ord, Read, Show)

-- | Differentiates between a /Father/ or /Mother/ Knows Best State.
data FatherOrMother = Father | Mother
    deriving (Eq, Ord, Read, Show)


readWACategory :: String -> Maybe WACategory
readWACategory s = case s of
    "Anarchy" -> Just Anarchy
    "Authoritarian Democracy" -> Just AuthoritarianDemocracy
    "Benevolent Dictatorship" -> Just BenevolentDictatorship
    "Capitalist Paradise" -> Just CapitalistParadise
    "Capitalizt" -> Just Capitalizt
    "Civil Rights Lovefest" -> Just CivilRightsLovefest
    "Compulsory Consumerist State" -> Just CompulsoryConsumeristState
    "Conservative Democracy" -> Just ConservativeDemocracy
    "Corporate Bordello" -> Just CorporateBordello
    "Corporate Police State" -> Just CorporatePoliceState
    "Corrupt Dictatorship" -> Just CorruptDictatorship
    "Democratic Socialists" -> Just DemocraticSocialists
    "Father Knows Best State" -> Just $ FatherKnowsBestState Father
    "Free-Market Paradise" -> Just FreeMarketParadise
    "Inoffensive Centrist Democracy" -> Just InoffensiveCentristDemocracy
    "Iron Fist Consumerists" -> Just IronFistConsumerists
    "Iron Fist Socialists" -> Just IronFistSocialists
    "Left-Leaning College State" -> Just LeftLeaningCollegeState
    "Left-wing Utopia" -> Just LeftWingUtopia
    "Liberal Democratic Socialists" -> Just LiberalDemocraticSocialists
    "Libertarian Police State" -> Just LibertarianPoliceState
    "Moralistic Democracy" -> Just MoralisticDemocracy
    "Mother Knows Best State" -> Just $ FatherKnowsBestState Mother
    "New York Times Democracy" -> Just NewYorkTimesDemocracy
    "Psychotic Dictatorship" -> Just PsychoticDictatorship
    "Right-wing Utopia" -> Just RightWingUtopia
    "Scandinavian Liberal Paradise" -> Just ScandinavianLiberalParadise
    "Tyranny by Majority" -> Just TyrannyByMajority
    _ -> Nothing

showWACategory :: WACategory -> String
showWACategory c = case c of
    Anarchy -> "Anarchy"
    AuthoritarianDemocracy -> "Authoritarian Democracy"
    BenevolentDictatorship -> "Benevolent Dictatorship"
    CapitalistParadise -> "Capitalist Paradise"
    Capitalizt -> "Capitalizt"
    CivilRightsLovefest -> "Civil Rights Lovefest"
    CompulsoryConsumeristState -> "Compulsory Consumerist State"
    ConservativeDemocracy -> "Conservative Democracy"
    CorporateBordello -> "Corporate Bordello"
    CorporatePoliceState -> "Corporate Police State"
    CorruptDictatorship -> "Corrupt Dictatorship"
    DemocraticSocialists -> "Democratic Socialists"
    FatherKnowsBestState Father -> "Father Knows Best State"
    FatherKnowsBestState Mother -> "Mother Knows Best State"
    FreeMarketParadise -> "Free-Market Paradise"
    InoffensiveCentristDemocracy -> "Inoffensive Centrist Democracy"
    IronFistConsumerists -> "Iron Fist Consumerists"
    IronFistSocialists -> "Iron Fist Socialists"
    LeftLeaningCollegeState -> "Left-Leaning College State"
    LeftWingUtopia -> "Left-wing Utopia"
    LiberalDemocraticSocialists -> "Liberal Democratic Socialists"
    LibertarianPoliceState -> "Libertarian Police State"
    MoralisticDemocracy -> "Moralistic Democracy"
    NewYorkTimesDemocracy -> "New York Times Democracy"
    PsychoticDictatorship -> "Psychotic Dictatorship"
    RightWingUtopia -> "Right-wing Utopia"
    ScandinavianLiberalParadise -> "Scandinavian Liberal Paradise"
    TyrannyByMajority -> "Tyranny by Majority"


readWAVote :: String -> Maybe (Maybe Bool)
readWAVote s = case s of
    "UNDECIDED" -> Just Nothing
    "FOR" -> Just $ Just True
    "AGAINST" -> Just $ Just False
    _ -> Nothing

showWAVote :: Maybe Bool -> String
showWAVote v = case v of
    Nothing -> "UNDECIDED"
    Just True -> "FOR"
    Just False -> "AGAINST"
