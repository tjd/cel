
(defun semantics_n ()

(mcon 'nation
      '((synonyms (country state sovereign-nation republic commonwealth kingdom empire))
	(plural (nations))
       )
)
(mcon 'nature
      '((synonyms (reality universe world))
       )
)
(mcon 'nature-of
      '((synonyms (character-of quality-of constitution-of characteristics-of temperament-of disposition-of temper-of))
	(plural (natures-of))
       )
)
(mcon 'near
      '((synonyms (close nearby near-to proximate))
	(antonyms (far))
       )
)
(mcon 'nearly
      '((synonyms (near closely almost virtually approximately))
	(antonyms (entirely exactly))
       )
)
(mcon 'necessary
      '((synonyms (essential important indispensable unavoidable))
	(antonyms (incidental nonessential unnecessary))
       )
)
(mcon 'necklace
      '((superordinates (adornment jewelry))
	(subordinates (beads chain choker string))
	(plural (necklaces))
       )
)
(mcon 'need
      '((synonyms (demand lack necessitate require want))
	(tenses (needs needed needing))
       )
)
(mcon 'neglect
      '((synonyms (disregard forget ignore omit overlook))
	(tenses (neglects neglected neglecting))
       )
)
(mcon 'neigh
      '((synonyms (whinny bray))
	(tenses (neighs neighed neighing))
       )
)
(mcon 'neighbor-of
      '((synonyms (neighbor borderer-of))
	(antonyms (distant-from))
	(plural (neighbors-of))
       )
)
(mcon 'nest
      '((synonyms (roost aviary beehive hive))
	(plural (nests))
       )
)
(mcon 'nest-of
      '((synonyms (nest roost-of home-of hive-of))
	(plural (nests-of))
       )
)
(mcon 'nest-on
      '((superordinates (inhabit))
	(synonyms (occupy reside-on live-on))
	(tenses (nests-on nested-on nesting-on))
       )
)
(mcon 'net
      '((superordinates (cloth fabric material structure textile))
	(subordinates (strainer))
	(synonyms (mesh web))
	(plural (nets))
       )
)
(mcon 'niece
      '((superordinates (person relation kin woman female))
	(synonyms (brothers-daughter sisters-daughter))
	(plural (nieces))
       )
)
(mcon 'night
      '((superordinates (time))
	(synonyms (nighttime darkness))
	(antonyms (day))
	(sub-parts (evening nightfall sunset sundown dusk midnight))
       )
)
(mcon 'nightingale
      '((superordinates (thrush bird))
	(plural (nightingales))
       )
)
(mcon 'noble
      '((synonyms (dignified ethical gallant handsome high-minded honorable imposing kinglike kingly lofty lord magnanimous magnificent majestic nobleman peer princely principled proud regal scrupulous splendid stately superb unapproachable))
	(antonyms (humble ignoble immoral nonroyal ungenerous unimpressive unroyal))
       )
)
(mcon 'nobleman
      '((superordinates (noble person male man human-being aristocrat patrician))
	(synonyms (lord noble))
	(subordinates (baron count earl duke grandee marquis viscount thane))
	(plural (noblemen))
       )
)
(mcon 'nuisance
      '((synonyms (annoyance pest bother trouble problem bore))
	(plural (nuisances))
       )
)
(mcon 'nuisance-to
      '((synonyms (nuisance annoyance-to bother-to problem-to))
	(plural (nuisances-to))
       )
)
) ; end of semantics_n

(defun semantics_o ()

(mcon 'oak
      '((superordinates (tree lumber timber wood))
	(subordinates (cork-oak pin-oak silk-oak))
	(synonyms (oak-tree))
	(plural (oaks))
       )
)
(mcon 'obey
      '((synonyms (adhere-to apply be-dutiful-to carry-out comply conform conform-to follow follow-orders fulfill heed keep mind observe perform practice respect revere serve))
	(tenses (obeys obeyed obeying))
       )
)
(mcon 'obtain
      '((superordinates (material-transfer))
	(synonyms (get procure secure receive take acquire gain))
	(tenses (obtains obtained obtaining))
       )
)
(mcon 'occur
      '((synonyms (come-about happen take-place transpire))
	(tenses (occurs occurred occurring))
       )
)
(mcon 'occur-always
      '((synonyms (occur happen-always transpire-always))
	(tenses (occurs-always occurred-always occurring-always))
       )
)
(mcon 'occur-never
      '((synonyms (occur happen-never transpire-never))
	(tenses (occurs-never occurred-never occurring-never))
       )
)
(mcon 'occur-often
      '((synonyms (occur happen-often transpire-often))
	(tenses (occurs-often occurred-often occurring-often))
       )
)
(mcon 'occur-sometimes
      '((synonyms (occur happen-sometimes transpire-sometimes))
	(tenses (occurs-sometimes occurred-sometimes occurring-sometimes))
       )
)
(mcon 'ocean
      '((synonyms (sea))
	(antonyms (land air))
	(plural (oceans))
       )
)
(mcon 'offend
      '((synonyms (anger disgust displease insult outrage revolt shock))
	(tenses (offends offended offending))
       )
)
; Look out for inconsistencies: offer vs offer-to (f41-8) and offers
(mcon 'offer
      '((synonyms (give hold-out make-available present proffer suggest supply tender))
	(tenses (offers offered offering))
       )
)
(mcon 'offer-that
      '((synonyms (offer))
	(tenses (offers-that offered-that offering-that))
       )
)
(mcon 'offer-to
      '((synonyms (offer proffer-to present-to give-to))
	(tenses (offers-that offered-that offering-that))
       )
)
(mcon 'official
      '((superordinates (administrator authority director expert manager))
	(synonyms (arbiter authorized bureaucrat civil-servant conventional formal judge office-holder officer referee regular umpire))
	(plural (officials))
       )
)
(mcon 'old
      '((synonyms (aged ancient antiquated dated obsolete elderly venerable mature worn))
	(antonyms (current early new young fresh immature modern))
       )
)
(mcon 'on
      '((synonyms (upon atop on-top-of over))
	(antonyms (not-on off))
       )
)
(mcon 'once
      '((synonyms (one-time))
	(antonyms (often more-than-once))
       )
)
(mcon 'oppose
      '((synonyms (clash compete conflict contend contradict differ interfere resist struggle))
	(tenses (opposes opposed opposing))
       )
)
(mcon 'order
      '((synonyms (command direction instruction decree prescription))
	(plural (orders))
       )
)
(mcon 'order-to
      '((synonyms (order command-to bid-to direct-to instruct-to))
	(tenses (orders-to ordered-to ordering-to))
       )
)
(mcon 'outlaw
      '((superordinates (bad-person))
        (synonyms (criminal crook felon lawbreaker malefactor offender villain wrongdoer))
        (subordinates (abductor captor crimp crimper kidnapper seizer shanghaier accessory accomplice coconspirator collaborator confederate conspirator partner_in_crime cutthroat gunman highbinder murderer cutthroat hired_gun hit_man murderer torpedo gan

gster hood hoodlum racketeer thug insurgent mutineer rebel revolutionary mugger robber raper rapist thief pirate))
	(plural (outlaws))
       )
)
(mcon 'outnumber
      '((synonyms (exceed outweigh predominate preponderate))
	(tenses (outnumbers outnumbered outnumbering))
       )
)
(mcon 'overhear
      '((synonyms (hear find-out))
	(antonyms (miss))
	(tenses (overhears overheard overhearing))
       )
)
(mcon 'overrun
      '((synonyms (deluge invade overpower overwhelm swamp))
	(antonyms (unoccupied))
	(tenses (overruns overran overrunning))
       )
)
(mcon 'overthrow
      '((synonyms (conquer defeat vanquish topple overcome depose
		   oust dethrone revolt rebel insurrect))
	(tenses (overthrows overthrew overthrowing))
       )
)
(mcon 'overwhelm
      '((synonyms (awe crush defeat devastate overpower overrun overthrow quell smother stifle subdue supress overturn topple))
	(tenses (overwhelms overwhelmed overwhelming))
       )
)
(mcon 'owe
      '((synonyms (be-indebted))
	(tenses (owes owed oweing))
       )
)
(mcon 'owner
      '((superordinates (businessman businessperson businesswoman entrepreneur industrialist merchant))
	(synonyms (proprietor possessor))
	(plural (owners))
       )
)
(mcon 'ox
      '((superordinates (bovid cattle mammal))
	(subordinates (bison buffalo cattle wild-ox yak))
	(synonyms (bovine steer))
	(plural (oxen))
       )
)
) ; end of semantics_o

(defun semantics_p ()

(mcon 'page
      '((superordinates (servant))
	(synonyms (attendant usher squire gofer))
       )
)
(mcon 'pap
      '((superordinates (food))
	(sub-parts (milk bread))
       )
)
(mcon 'pardon
      '((synonyms (absolve acquit clear condone countenance excuse forgive make-allowance put-up-with remit tolerate))
	(tenses (pardons pardoned pardoning))
       )
)
(mcon 'parrot
      '((superordinates (aves bird))
	(subordinates (budgie cockatoo lovebird macaw parakeet))
	(plural (parrots))
       )
)
(mcon 'part-of
      '((synonyms (piece portion fraction part))
	(subordinates (half))
	(plural (parts-of))
       )
)
(mcon 'partake-of
      '((synonyms (partake drink eat))
	(tenses (partakes-of partook-of partaking-of))
       )
)
(mcon 'participate
      '((synonyms (enlist partake share take-part))
        (tenses (participates participated participating))
       )
)
(mcon 'participate-in
      '((synonyms (participate associate-in engage-in enlist-in partake-of share-in take-part-in))
        (tenses (participates-in participated-in participating-in))
       )
)
(mcon 'partridge
      '((superordinates (bird fowl))
	(plural (partridges))
       )
)
(mcon 'pass-through
      '((synonyms (pass traverse go-through move-through))
	(tenses (passes-through passed-through passing-through))
       )
)
(mcon 'passion
      '((superordinates (emotion feeling love mood))
        (synonyms (ardor disposition fervor humor mood temper))
        (plural (passions))
       )
)
(mcon 'passion-of
      '((superordinates (emotion-of feeling-of love-of mood-of))
	(synonyms (passion ardor-of disposition-of fervor-of humor-of mood-of temper-of))
	(plural (passions-of))
       )
)
(mcon 'patient
      '((superordinates (normal-person))
	(synonyms (sick-person invalid case sufferer inpatient outpatient))
       )
)
(mcon 'pay
      '((synonyms (settle repay remunerate reimburse give))
	(tenses (pays paid paying))
       )
)
(mcon 'peace
      '((superordinates (happiness))
	(synonyms (calm calmness peacefulness quiet quietude repose tranquillity))
	(antonyms (war fighting unrest disquiet))
       )
)
(mcon 'pebble
      '((synonyms (stone gravel slingstone))
	(plural (pebbles))
       )
)
(mcon 'perform
      '((synonyms (accomplish achieve act act-out adhere-to apply behave bring-about carry-out commit complete conform-to consummate delineate depict discharge do effect enact engage-in execute express fulfill function heed impersonate implement indulge-i

n interpret keep make-good obey observe operate perpetrate personate play portray practice present procure pull pull-off realize render represent run transact undertake work put-on stage))
	(tenses (performs performed performing))
       )
)
(mcon 'permit
      '((synonyms (admit-of allow approve authorize bear brook cede clear concede consent consent-to countenance encourage endorse establish give-leave give-over give-up grant justify leave let maintain make-possible pass sanction stand-for suffer surrend

er tolerate warrant yield))
	(tenses (permits permited permitting))
       )
)
(mcon 'person
      '((superordinates (living-thing organism mammal animal))
	(subordinates (adult animal-tending-person authority bad-person craftsman educator entertainer female male military-personnel noble normal-person official professional-person public-servant race-of-person religious-person ruler sportsman unfortunate-pers

on unpleasant-person worker working-person young-person))
	(synonyms (human man woman human-being))
	(sub-parts (anatomy body carcass character human-body intestines membrane personality self structure tooth))
	(plural (people persons))
       )
)      
(mcon 'persist
      '((synonyms (abide continue endure last persevere remain stay))
	(tenses (persists persisted persisting))
       )
)
(mcon 'persuade
      '((synonyms (influence induce compel influence cajole seduce urge plead importune solicit entreat convince induce cause))
	(tenses (persuades persuaded persuading))
       )
)
(mcon 'physician
      '((superordinates (professional-person public-servant))
	(subordinates (abortionist anesthesiologist baby-doctor brain-doctor cardiologist chiropodist coroner dental-surgeon dentist dermatologist diagnostician ear-doctor ear-specialist ent-man eye-ear-nose-and-throat-doctor eye-doctor foot-doctor general-pract

itioner geriatrician gerontologist gp gynecologist heart-specialist heart-surgeon horse-doctor house-physician immunologist intern internist medical-examiner neurologist obstetrician oculist operating-surgeon opthalmologist orthodontist orthopedist otolar

yngologist otologist otorhinolaryngologist pathologist pediatrician pediatrist periodontist podiatrist psychiatrist radiologist resident resident-physician rhinolaryngologist sawbones serologist shrink skin-doctor surgeon urologist vet veterinarian veteri

nary veterinary-surgeon))
	(synonyms (doc doctor medic medical-man medical-practitioner))
	(plural (physicians))
       )
)
(mcon 'pick-up
      '((synonyms (lift grab carry collect escort))
	(tenses (picks-up picked-up picking-up))
       )
)
(mcon 'picture
      '((superordinates (representation))
        (subordinates (cartoon diorama drawing film painting panorama photo photocopy photograph short-subject shot sketch snapshot))
        (synonyms (portrait))
        (plural (pictures))
       )
)
(mcon 'picture-of
      '((superordinates (representation-of))
	(subordinates (cartoon-of drawing-of film-of painting-of photo-of photocopy-of photograph-of sketch-of snapshot-of))
	(synonyms (portrait-of))
        (plural (pictures-of))
       )
)
(mcon 'pigeon
      '((superordinates (bird fowl))
	(subordinates (dove squab))
	(synonyms (dove))
	(plural (pigeons))
       )
)
(mcon 'pigeonry
      '((superordinate (coop))
	(synonyms (pigeon-house))
	(sub-parts (pigeons))
	(plural (pigeonries))
       )
)
(mcon 'pirate
      '((superordinates (mariner bad-person seaman))
	(synonyms (corsair buccaneer privateer))
	(plural (pirates))
       )
)
(mcon 'pity
      '((superordinates (confidence sympathy))
	(synonyms (care-for feel-for support sympathize-with))
	(tenses (pities pitied pitying))
       )
)
(mcon 'place-in
      '((synonyms (place put put-in insert-in))
	(tenses (places-in place-in placing-in))
       )
)
(mcon 'place-on
      '((synonyms (place lay-on place-on put-on))
	(antonyms (take-off remove-from))
	(tenses (places-on place-on placing-on))
       )
)
(mcon 'plain
      '((synonyms (apparent artless austere bare candid distinct evident explicit forthright intelligible lucid observable obvious palpable prosaic readable simple straightforward unadorned undecorated unembellished))
	(antonyms (adorned complex fancy indirect obscure))
       )
)
(mcon 'plan
      '((synonyms (intention plot scheme idea method))
	(plural (plans))
       )
)
(mcon 'plan-for
      '((synonyms (plan arrange-for prepare-for))
	(tenses (plans-for planned-for planning-for))
       )
)
(mcon 'plan-of
      '((synonyms (plan intention-of scheme-of idea-of))
	(plural (plans-of))
       )
)
(mcon 'plan-to
      '((synonyms (plan intend-to purpose-to))
	(tenses (plans-to planned-to planning-to))
       )
)
(mcon 'planned
      '((synonyms (calculated considered deliberate designed due fixed intended intentional mapped-out methodical ordered organized plotted premeditated prepared preplanned purposed regimented scheduled))
	(antonyms (accidental aimless irregular purposeless uncharted unexpected unplanned unpremeditated unsystematic))
       )
)
; PLAY looks bad -- noun and verb mishmosh.
(mcon 'play
      '((synonyms (act drama enactment feign frolic perform performance portrayal presentation romp simulation story pageant show sound stage-show use))
	(subordinates (tragedy comedy tragicomedy mystery miracle-play morality-play passion-play pastoral masque charade pantomime experimental-drama musical psycho-drama problem-play history melodrama))
	(plural (plays))
	(tenses (plays played playing))
       )
)
; Note: this is play only as a noun.  Wordnet is chock full of info about play
;  as a verb, but i never use it that way. Wordnet has no info about play as
;  a noun.
(mcon 'plead
      '((synonyms (adjure allege answer apologize appeal argue ask ask-for aver beg beseech bet cite claim coax contend declare defend entreat implore importune insist invoke maintain persuade petition plead-with pray press protest request solicit supplic

ate try-to-prove urge))
	(tenses (pleads pleaded pleading))
       )
)
(mcon 'plead-with
      '((synonyms (plead coax entreat importune persuade urge))
	(tenses (pleads-with pleaded-with pleading-with))
       )
)
(mcon 'pleased
      '((synonyms (happy content amused satisfied gratified))
	(antonyms (displeased discontented unhappy sorrowful))
       )
)
(mcon 'pledge-that
      '((synonyms (pledge promise-that vow-that guarantee-that))
	(tenses (pledges-that pledged-that pledging-that))
       )
)
(mcon 'plot
      '((synonyms (plan scheme design idea))
	(plural (plots))
       )
)
(mcon 'plot-against
      '((synonyms (plot conspire collude scheme-against))
	(tenses (plots-against plotted-against plotting-against))
       )
)
(mcon 'plot-to
      '((synonyms (plot plan-to conspire-to devise-to intend-to))
	(tenses (plots-to plotted-to plotting-to))
       )
)
(mcon 'poem
      '((synonyms (verse rhyme jingle))
	(plural (poems))
       )
)
(mcon 'poison
      '((superordinates (kill))
        (tenses (poisons poisoned poisoning))
       )
)
(mcon 'police
      '((synonyms (constabulary guard patrol watch))
	(subordinates (state-police troopers highway-patrol county-police provincial-police security-force special-police riot-police secret-police))
	(plural (police))
       )
)
(mcon 'political
      '((synonyms (civic governmental civil public-affairs))
	(antonyms (non-political))
       )
)
; POND - c'est jolie, n'est pas?
(mcon 'pond
      '((superordinates (body-of-water))
	(synonyms (lake pool puddle))
	(sub-parts (water lilypads frogs))
	(plural (ponds))
       )
)
(mcon 'pool
      '((superordinates (body-of-water))
	(synonyms (lake pond puddle swimming-pool))
	(sub-parts (water))
	(plural (pools))
       )
)
(mcon 'poor
      '((synonyms (deprived humble inferior inopportune lowly meager miserable shabby underprivileged unfortunate wretched))
	(antonyms (fortunate rich))
       )
)
(mcon 'port
      '((synonyms (harbor haven seaport))
	(sub-parts (anchorage))
	(plural (ports))
       )
)
(mcon 'possess
      '((synonyms (have own hold control))
	(tenses (posesses possessed possessing))
       )
)
(mcon 'possible
      '((synonyms (potential plausible feasible likely probable practical practicable realistic))
	(antonyms (impossible unlikely implausible impractical unrealistic unthinkable))
       )
)
(mcon 'power
      '((synonyms (potency force might vigor strength influence authority clout prestige control mastery command))
	(antonyms (weakness))
        (plural (powers))
       )
)
(mcon 'power-of
      '((synonyms (potency-of force-of might-of vigor-of strength-of influence-of authority-of clout-of prestige-of))
	(antonyms (weakness-of))
        (plural (powers-of))
       )
)
(mcon 'powerful
      '((synonyms (forceful mighty potent puissant strong))
	(antonyms (impotent powerless soft weak))
       )
)
(mcon 'powerless
      '((synonyms (helpless impotent ineffective unable weak))
	(antonyms (able capable effective potent powerful strong))
       )
)
(mcon 'prankster
      '((superordinates (unpleasant-person bad-person))
	(synonyms (mischief-maker rogue knave rascal scamp wag rapscallion joker jokester practical-joker))
	(plural (pranksters))
       )
)
(mcon 'prefer
      '((synonyms (fancy favor))
	(tenses (prefers preferred preferring))
       )
)
(mcon 'prefer-to
      '((synonyms (prefer favor-over))
	(tenses (prefers-to preferred-to preferring-to))
       )
)
(mcon 'preferable
      '((synonyms (choice desirable))
        (antonyms (undesirable))
       )
)
(mcon 'preferable-to
      '((synonyms (preferable))
       )
)
(mcon 'prepare
      '((synonyms (plan organize ready make-ready))
	(tenses (prepares prepared preparing))
       )
)
(mcon 'prepare-for
      '((synonyms (prepare plan-for organize-for make-ready-for))
	(tenses (prepares-for prepared-for preparing-for))
       )
)
(mcon 'prepared
      '((synonyms (ready))
	(antonyms (unprepared unfit))
       )
)
(mcon 'prescribe
      '((synonyms (advise command decree dictate-that ordain ordain-that order-that prescribe-that))
        (tenses (prescribes prescribed prescribing))
       )
)
(mcon 'prescribe-for
      '((synonyms (prescribe give-to recommend-for))
        (tenses (prescribes-for prescribed-for prescribing-for))
       )
)
(mcon 'present
      '((synonyms (award gratuity donation endowment gift offer))
        (plural (presents))
       )
)
(mcon 'present-at
      '((synonyms (at at-hand-at by))
        (antonyms (absent absent-from))
       )
)
(mcon 'prestige
      '((synonyms (respect regard approval reverence honor)))
)
(mcon 'pretend
      '((synonyms (feign simulate counterfeit act bluff mislead deceive))
	(tenses (pretends pretended pretending))
       )
)
(mcon 'pretend-that
      '((synonyms (pretend feign-that act-as-if))
	(tenses (pretends-that pretended-that pretending-that))
       )
)
(mcon 'pretense
      '((synonyms (affectation airs hypocrisy pretension ostentation pretext show sham excuse))
        (plural (pretenses))
       )
)
(mcon 'pretense-of
      '((synonyms (pretense airs-of hypocrisy-of pretext-of excuse-of))
        (plural (pretenses-of))
       )
)
(mcon 'pretext
      '((synonyms (reason rationale excuse pretense front))
	(plural (pretexts))
       )
)
(mcon 'pretty
      '((synonyms (appealing attractive beautiful bonny charming comely fair likeable lovely))
        (antonyms (displeasing ugly unattractive))
       )
)
(mcon 'prettier
      '((superordinates (more pretty))
	(synonyms (comlier fairer lovlier))
	(antonyms (uglier))
       )
)
(mcon 'prevail
      '((synonyms (continue dominate predominate reign rule triumph win))
	(tenses (prevails prevailed prevailing))
       )
)
(mcon 'prey
      '((superordinates (animal beast creature fauna))
	(synonyms (game loot pillage quarry target))
	(plural (preys))
       )
)
; Problem: no appropriate noun synonyms -- how shall we indicate that
;   prince is the son of the king?
(mcon 'prince
      '((superordinates (boy nobleman son ruler lord))
	(plural (princes))
       )
)
(mcon 'princess
      '((superordinates (female girl aristocrat ruler noblewoman))
	(synonyms (crown-princess queen empress czarina rani maharani grand-duchess sovereign-princess queen-regent infanta princesse))
	(plural (princesses))
       )
)
(mcon 'prize
      '((synonyms (booty spoil spoils loot))
	(plural (prizes))
       )
)
(mcon 'problem
      '((synonyms (difficulty trouble matter inconvenience bother annoyance))
	(plural (problems))
       )
)
(mcon 'produce
      '((synonyms (give-rise-to yield emit originate make))
       )
)
(mcon 'project
      '((synonyms (design idea invention plan task undertaking))
	(plural (projects))
       )
)
(mcon 'promise
      '((synonyms (assurance pledge vow))
	(plural (promises))
       )
)
(mcon 'promise-that
      '((synonyms (promise pledge-that vow-that))
	(tenses (promises-that promised-that promising-that))
       )
)
(mcon 'promise-to
      '((synonyms (promise promise-that pledge-to vow-to))
	(tenses (promises-to promised-to promising-to))
       )
)
(mcon 'prophecy
      '((superordinates (statement declaration))
	(synonyms (prediction forecast promise omen foresight fortune oracle revelation))
	(plural (prophesies))
       )
)
(mcon 'prospect
      '((synonyms (possibility))
	(plural (prospects))
       )
)
(mcon 'protect
      '((synonyms (accompany aid assist buffer cherish defend escort guard harbor help keep-safe nurture preserve safeguard shelter shield))
        (tenses (protects protected protecting))
       )
)
(mcon 'protect-from
      '((synonyms (protect buffer-from defend-against guard-against keep-safe-from safeguard-against shelter-against shelter-from shield-from))
        (tenses (protects-from protected-from protecting-from))
       )
)
(mcon 'proud
      '((synonyms (dignified lofty majestic noble stately))
	(antonyms (humble unimpressive))
       )
)
(mcon 'proud-of
      '((synonyms (proud happy-with satisfied-with))
	(antonyms (dissatisfied-with unhappy-with))
       )
)
(mcon 'provoke
      '((synonyms (aggravate agitate anger annoy arouse bother chafe challenge defy disturb exasperate excite fret gall goad harrass incense irk irritate mock outrage rouse stimulate taunt tease upset vex))
	(tenses (provokes provoked provoking))
       )
)
(mcon 'puff-up
      '((synonyms (distend enlarge inflate puff-out swell swell-up))
	(tenses (puffs-up puffed-up puffing-up))
       )
)
(mcon 'punish
      '((synonyms (afflict beat castigate censure chasten chastise correct curse discipline flog grieve harass hurt injure lash oppress penalize persecute rebuke reprove scourge switch whip))
	(tenses (punishes punished punishing))
       )
)
(mcon 'punish-for
      '((synonyms (punish castigate-for censure-for chastise-for discipline-for flog-for penalize-for rebuke-for reprove-for))
	(tenses (punishes-for punished-for punishing-for))
       )
)
(mcon 'purse
      '((superordinates (bag))
	(synonyms (money-bag handbag pocketbook))
	(plural (purses))
       )
)
(mcon 'pursue
      '((synonyms (chase continue dog engage-in follow go-after harass harry hound hunt persecute press-on proceed-with prosecute seek seek-after shadow tail trace track trail try-to-overtake))
	(tenses (pursues pursued pursuing))
       )
)
(mcon 'put-in
      '((synonyms (place put place-in install set-up))
	(tenses (puts-in put-in putting-in))
       )
)
(mcon 'put-on
      '((synonyms (assume don dress place place-on present produce put wear))
	(antonyms (take-off))
	(tenses (puts-on put-on putting-on))
       )
)
(mcon 'put-upon
      '((synonyms (put place-on lay-on))
	(antonyms (take-off remove-from))
	(tenses (puts-upon put-upon putting-upon))
       )
)
) ; end of semantics_p

(defun semantics_qr ()

; QUARREL - noun-verb conflict.
(mcon 'quarrel
      '((synonyms (argument brawl clash complaint contention disagreement dispute row spar squabble))
	(plural (quarrels))
       )
)
(mcon 'quarrel
      '((synonyms (agitate argue bicker brawl carp cavil chide clamor clash complain contend disagree dispute find-fault row spar squabble wrangle))
	(tenses (quarrels quarreled quarrelling))
       )
)
(mcon 'quarrel-between
      '((synonyms (quarrel argument-between clash-between contention-between disagreement-between dispute-between))
	(plural (quarrels-between))
       )
)
(mcon 'queen
      '((superordinates (noblewoman ruler wife))
	(plural (queens))
       )
)
; Problem: no appropriate noun synonyms -- how shall we indicate that
;   queen is the wife of the king?
(mcon 'race
      '((synonyms (competition contest dash run))
	(plural (races))
       )
)
(mcon 'radiation
      '((subordinates (light ultraviolet infrared visible-light
		       X-ray gamma-ray solar-ray cosmic-ray))
	(synonyms (electro-magnetic-radiation radiant-energy
		   radioactivity))
       )
)
; Note: the usage for 'raise' in the plays is in bringing up a child.
; Note: it is in the fables, too, so I'm just going to pare it down to this.
(mcon 'raise
      '((synonyms (arouse breed bring-up cultivate educate grow hoist increase instruct lift nourish nurture rear))
	(tenses (raises raised raising))
       )
)
(mcon 'ram
      '((superordinates (sheep))
	(synonyms (lamb teg))
	(plural (rams))
       )
)
(mcon 'rascal
      '((synonyms (rogue devil knave rowdy hoodlum imp))
	(plural (rascals))
       )
)
(mcon 'rat
      '((superordinates (rodent mammal))
	(subordinates (bandicoot-rat brown-rat cotton-rat jerboa-rat wharf-rat))
	(plural (rats))
       )
)
(mcon 'ray
      '((synonyms (beam vector))
	(subordinates (light-ray X-ray gamma-ray solar-ray cosmic-ray))
	(plural (rays))
       )
)
(mcon 'ray-source
      '((superordinates (device radiation-source))
	(synonyms (ray-gun))
	(plural (ray-sources))
       )
)
(mcon 'real
      '((synonyms (actual authentic embodied existent factual genuine honest material measurable physical tangible true valid veritable veridical))
	(antonyms (counterfeit false incorporeal intangible surreal unreal))
       )
)
(mcon 'realize
      '((synonyms (discern learn perceive recognize see understand actualize accomplish))
	(tenses (realizes realized realizing))
       )
)
(mcon 'realize-that
      '((synonyms (realize discern-that discover-that learn-that perceive-that recognize-that see-that understand-that))
	(tenses (realizes-that realized-that realizing-that))
       )
)
(mcon 'reap
      '((synonyms (crop cut gather harvest mow receive take))
	(tenses (reaps reaped reaping))
       )
)
(mcon 'rear
      '((synonyms (breed bring-up educate instruct nourish nurture raise))
	(tenses (rears reared rearing))
       )
)
(mcon 'reason
      '((synonyms (cause explanation justification))
	(plural (reasons))
       )
)
(mcon 'reason-for
      '((synonyms (reason cause-for explanation-for justification-for))
	(plural (reasons-for))
       )
)      
(mcon 'rebuke
      '((synonyms (bawl-out berate castigate censure chastise chide criticize lecture punish reprimand reproach scold vituperate))
        (tenses (rebukes rebuked rebuking))
       )
)
(mcon 'rebuke-for
      '((synonyms (rebuke berate-for castigate-for censure-for chastise-for chide-for criticize-for punish-for reprimand-for reproach-for scold-for))
        (tenses (rebukes-for rebuked-for rebuking-for))
       )
)
(mcon 'receive
      '((synonyms (accept acquire gain get obtain))
	(antonyms (bestow confer dispense give))
	(tenses (receives received receiving))
       )
)
; Note:  I'm paring down recognize.
(mcon 'recognize
      '((synonyms (acknowledge appreciate comprehend discern identify know learn notice perceive realize see understand))
	(tenses (recognizes recognized recognizing))
       )
)
(mcon 'reflection
      '((synonyms (image likeness mirror-image picture))
        (plural (reflections))
       )
)
(mcon 'reflection-of
      '((synonyms (reflection image-of likeness-of mirror-image-of picture-of))
        (plural (reflections-of))
       )
)
(mcon 'refuse
      '((synonyms (decline deny disallow dismiss reject repudiate spurn turn-down withhold-compliance withhold-permission))
	(antonyms (accept))
	(tenses (refuses refused refusing))
       )
)
(mcon 'refuse-to
      '((synonyms (refuse decline-to))
	(antonyms (agree-to))
	(tenses (refuses-to refused-to refusing-to))
       )
)
(mcon 'regret
      '((synonyms (lament-that rue))
	(tenses (regrets regretted regretting))
       )
)
(mcon 'rehearse
      '((synonyms (detail enumerate list narrate practice recite recount relate repeat report restate say-again state tell tell-again train))
	(tenses (rehearses rehearsed rehearsing))
       )
)
(mcon 'reject
      '((synonyms (abjure decline deny discard disinherit dismiss disown ditch forswear ignore rebuff refuse renounce repel scorn spurn))
	(tenses (rejects rejected rejecting))
       )
)
(mcon 'rejoice
      '((synonyms (celebrate jubilate regale))
        (tenses (rejoices rejoiced rejoicing))
       )
)
(mcon 'rejoice-about
      '((synonyms (rejoice celebrate celebrate-about regale-about))
        (tenses (rejoices-about rejoiced-about rejoicing-about))
       )
)
(mcon 'rejoin
      '((synonyms (join return-to))
	(tenses (rejoins rejoined rejoining))
       )
)
(mcon 'relative
      '((synonyms (kinsman kin family relation)) 
        (plural (relatives))
       )
)
(mcon 'relative-of
      '((synonyms (kinsman-of kin-of family-of relation-of))
        (plural (relatives-of))
       )
)
(mcon 'relax
      '((synonyms (loosen refresh slacken soften unwind))
	(tenses (relaxes relaxed relaxing))
       )
)
(mcon 'release
      '((synonyms (free liberate emancipate relinquish))
	(antonyms (retain detain))
	(tenses (releases released releasing))
       )
)
(mcon 'release-from
      '((synonyms (release free-from liberate-from emancipate-from))
	(tenses (releases-from released-from releasing-from))
       )
)
(mcon 'religious
      '((synonyms (churchly clerical devoted devout divine dutiful ecclesiastic ecclesiastical holy reverent rigid rigorous sacred scrupulous spiritual strict undeviating))
	(antonyms (impious inexact irreligious profane unspiritual worldly))
       )
)
(mcon 'relinquish
      '((synonyms (surrender resign))
	(antonyms (retain hold-on-to))
	(tenses (relinquishes relinquished relinquishing))
       )
)
(mcon 'remove
      '((synonyms (detach separate extract pull))
	(tenses (removes removed removing))
       )
)
(mcon 'repair
      '((synonyms (fix mend make_functional))
	(antonyms (break))
       )
)
(mcon 'repay
      '((synonyms (cast-back compensate compensate-for give-back hurl-back indemnify make-amends make-good make-up-for pay pay-back reciprocate recompense recoup redress reimburse render repair requite restore retaliate retort return))
	(tenses (repays repayed repaying))
       )
)
(mcon 'repent
      '((synonyms (be-sorry-for do-penance feel-contrition feel-repentance regret repent-of rue))
	(tenses (repents repented repenting))
       )
)
(mcon 'repentant
      '((synonyms (contrite remorseful))
	(antonyms (impenitent unrepentant))
       )
)
(mcon 'replace
      '((synonyms (change substitute exchange switch))
       )
)
(mcon 'reproach
      '((synonyms (bawl-out berate chide criticize lecture rebuke reprimand reprove scold))
        (tenses (reproaches reproached reproaching))
       )
)
(mcon 'reproach-for
      '((synonyms (reproach bawl-out-for berate-for chide-for criticize-for rebuke-for reprimand-for reprove-for scold-for))
        (tenses (reproaches-for reproached-for reproaching-for))
       )
)
(mcon 'request
      '((synonyms (ask demand solicit seek beseech))
	(tenses (requests requested requesting))
       )
)
(mcon 'request-of
      '((synonyms (request ask-of demand-of solicit-from seek-from))
	(tenses (requests-of requested-of requesting-of))
       )
)
(mcon 'rescue
      '((synonyms (aid deliver free help let-loose liberate loose ransom reclaim recover redeem release relieve restore restore-to-usefulness retrieve salvage save set-free succor))
	(tenses (rescues rescued rescuing))
       )
)
(mcon 'rescue-from
      '((synonyms (rescue deliver-from free-from liberate-from reclaim-from recover-from release-from save-from))
	(tenses (rescues-from rescued-from rescuing-from))
       )
)
(mcon 'resist
      '((synonyms (clash combat contend contest defy fight oppose rebel repulse struggle-against))
	(tenses (resists resisted resisting))
       )
)
(mcon 'resolve
      '((synonyms (arrange decide dispel reconcile rectify settle solve))
	(tenses (resolves resolved resolving))
       )
)
(mcon 'respect
      '((synonyms (esteem admiration regard))
	(antonyms (disrespect))
	(tenses (respects respected respecting))
       )
)
(mcon 'rest-of
      '((synonyms (rest remainder remainder-of remnant balance-of))
       )
)
(mcon 'retreat
      '((synonyms (back-down back-off fade fall-back give-ground leave recede retire run withdraw))
        (tenses (retreats retreated retreating))
       )
)
(mcon 'retreat-to
      '((synonyms (retreat fade-to fall-back-to retire-to run-to withdraw-to))
        (tenses (retreats-to retreated-to retreating-to))
       )
)
(mcon 'return
      '((synonyms (bring-back cast-back echo give-back hit-back hurl-back make-restitution pass-back put-back reciprocate repay reply respond retaliate retort revert send-back take-back take-in throw-back))
	(tenses (returns returned returning))
       )
)
(mcon 'return-to
      '((synonyms (go_back come_back go_back_again come_back_again begin-again recommence resume))
	(tenses (returns-to returned-to returning-to))
       )
)
(mcon 'reunited 
      '((synonyms (together-again brought-together reconciled))
       )
)
(mcon 'reveal
      '((synonyms (air attest bare betray bewray blab break bring-to-light clarify communicate confess convey convey-knowledge declare demonstrate disclose discover display divulge elucidate evidence evince exhibit explain expose illustrate impart indicat

e inform inform-on lay-bare lay-open make-clear make-evident make-intelligible make-known make-plain make-public make-understandable manifest open-to-view proclaim prove publicize report show show-up strip tattle tell testify transmit uncover unfold unmas

k utter))
	(tenses (reveals revealed revealing))
       )
)
(mcon 'revenge
      '((synonyms (retaliation satisfaction vengeance))
       )
)
(mcon 'revenge-on
      '((synonyms (revenge revenge-against retaliation-against vengeance-on))
       )
)
(mcon 'reverse
      '((synonyms (opposite converse contrary backwards))
	(antonyms (forward onward))
       )
)
(mcon 'revolt
      '((synonyms (rebel mutiny desert))
	(tenses (revolts revolted revolting))
       )
)
(mcon 'revolt-against
      '((synonyms (revolt rebel-against mutiny-against desert turn-against))
	(tenses (revolts-against revolted-against revolting-against))
       )
)
(mcon 'reward
      '((synonyms (payment recompense requite))
	(plural (rewards))
       )
)
(mcon 'rich
      '((synonyms (affluent prosperous monied wealthy ))
	(antonyms (poor))
       )
)
(mcon 'riddle
      '((superordinates (conundrum question))
	(plural (riddles))
       )
)
; RIDICULE - pared down.
(mcon 'ridicule
      '((synonyms (deride mock taunt tease))
	(tenses (ridicules ridiculed ridiculing))
       )
)
(mcon 'ridiculous
      '((synonyms (absurd crackpot farcical foolish impractical inept laughable ludicrous silly zany))
	(antonyms (reasonable sane))
       )
)
(mcon 'ring
      '((superordinates (adornment jewelry))
	(subordinates (boxing-ring engagement-ring nut signet-ring washer wedding-band wedding-ring wrestling-ring))
	(synonyms (band circle loop))
	(part-of (chain))
	(plural (rings))
       )
)
(mcon 'ripe
      '((synonyms (aged consummate developed mature mellowed perfected prime ripened))
	(antonyms (immature unready young))
       )
)
(mcon 'risky
      '((synonyms (dangerous hazardous chancy precarious))
	(antonyms (safe certain))
       )
)
(mcon 'rival
      '((superordinates (normal-person unpleasant-person))
        (subordinates (enemy foe opposition))
        (synonyms (adversary antagonist challenger competitor contender contestant opponent player))
        (plural (rivals))
       )
)
(mcon 'rivalry
      '((synonyms (competition contention jealousy))
	(antonyms (agreement settlement))
	(plural (rivalries))
       )
)
(mcon 'river
      '((superordinates (watercourse waterway))
	(synonyms (stream))
	(plural (rivers))
      )
)
(mcon 'road
      '((superordinates (path route))
	(subordinates (highway drive parkway railroad railway))
	(sub-parts (pavement paving roadbed shoulder))
	(plural (roads))
       )
)
(mcon 'rock
      '((superordinates (mineral))
	(subordinates (gem slate granite))
	(synonyms (boulder pebble stone))
	(plural (rocks))
       )
)
(mcon 'rocky
      '((synonyms (stony pebbly rock-strewn rubble-strewn craggy))
	(antonyms (smooth clean flat barren))
       )
)
(mcon 'rogue
      '((superordinates (unpleasant-person))
	(synonyms (scoundrel cad rascal scamp scalawag rapscallion))
	(plural (rogues))
       )
)
(mcon 'roots
      '((synonyms (radix radicle))
	(subordinates (rootlet taproot rhizome tuber bulb))
       )
)
(mcon 'rose
      '((superordinate (flower))
	(plural (roses))
       )
)
(mcon 'rotten
      '((superordinate (bad))
	(synonyms (foul rotted ruined sour spoiled))
	(antonyms (fresh good solid))
       )
)	
(mcon 'route
      '((superordinates (passage way))
	(subordinates (seaway thoroughfare road avenue trail walkway))
	(synonyms (path course))
	(plural (routes))
       )
)
(mcon 'rude
      '((synonyms (affronting artless backward bad-mannered bold boorish brash cheeky churlish cloddish clownish common crude crudely-shaped curmudgeonly discourteous distasteful dour familiar forward fresh gloomy glum homespun humble ill-bred ill-formed 

ill-mannered impertinent impolite impudent inartistic indelicate insulting inurbane loutish low-class lowly nervy obnoxious offensive outrageous plebeian preliterate primitive rough roughcast roughhewn saturnine saucy surly tasteless uncivil uncouth unfin

ished ungallant ungracious unpleasant unpolished unrefined vulgar))
	(antonyms (backward civil civilized courteous couth delicate even fine good-natured gracious high inoffensive noble polite refined respectful sensitive shy skilled smooth suave tasteful tasteless))
       )
)
(mcon 'ruin
      '((synonyms (blight break confound corrupt crush damage defeat degrade destroy devastate discredit dishonor impair impoverish injure pervert ravage spoil undo waste))
	(tenses (ruins ruined ruining))
       )
)
(mcon 'ruined
      '((synonyms (bankrupt broke broken defeated desolate failed hopeless undone))
	(antonyms (restored successful))
       )
)
(mcon 'rule
      '((synonyms (command control direct dominate govern influence preside-over reign-over rule-over))
	(tenses (rules ruled ruling))
       )
)
(mcon 'ruler
      '((superordinates (person human))
	(synonyms (governor lord master))
	(plural (rulers))
       )
)
(mcon 'ruler-of
      '((synonyms (ruler lord-of master-of liege-of))
	(plural (rulers-of))
       )
)
(mcon 'run
      '((synonyms (race dart dash sprint))
	(tenses (runs ran running))
       )
)
(mcon 'run-across
      '((synonyms (run race-across dash-across sprint-across rush-across))
	(tenses (runs-across ran-across running-across))
       )
)
(mcon 'run-to
      '((synonyms (run race-to go-to rush-to))
	(antonyms (run-from stay-away-from))
	(tenses (runs-to ran-to running-to))
       )
)
(mcon 'rush
      '((synonyms (bound charge dart dash fly hasten hurry hurtle race run scurry speed spring surge))
        (tenses (rushes rushed rushing))
       )
)
(mcon 'rush-into
      '((synonyms (rush bound-into charge-into dart-into dash-into fly-into hasten-into hurry-into hurtle-into race-into scurry-into spring-into))
        (tenses (rushes-into rushed-into rushing-into))
       )
)
) ; end of semantics_qr

