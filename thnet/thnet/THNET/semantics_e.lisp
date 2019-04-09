
(defun semantics_e ()

(mcon 'eagle
      '((superordinates (bird))
	(subordinates (eaglet bald-eagle))
	(plural (eagles))
       )
)
(mcon 'eaglet
      '((superordinates (eagle youth youngster child))
	(plural (eaglets))
       )
)
(mcon 'earl
      '((superordinates (nobleman aristocrat))
	(synonyms (count))
	(plural (earls))
       )
)
(mcon 'eat
      '((synonyms (consume devour dine feast ingest drink))
	(tenses (eats ate eating))
       )
)
(mcon 'effort
      '((synonyms (endeavor exertion attempt trial trouble pains))
	(plural (efforts))
       )
)
(mcon 'egg
      '((superordinates (food nutriment))
	(subordinates (boiled-egg deviled-egg fresh-egg fried-egg omelet poached-egg scrambled-egg))
	(plural (eggs))
       )
)
(mcon 'egg-of
      '((synonyms (egg egg-cell-of ovum-of))
	(plural (eggs-of))
       )
)
(mcon 'elect
      '((synonyms (appoint choose decide-upon designate name nominate pick select vote-for))
	(antonyms (impeach))
	(tenses (elects elected electing))
       )
)
(mcon 'elephant
      '((superordinates (proboscidean mammal))
	(sub-parts (trunk))
	(plural (elephants))
       )
)
(mcon 'elope
      '((synonyms (run-away-with abscond run-away run-away-together run-off))
	(tenses (elopes eloped eloping))
       )
)
(mcon 'empire
      '((synonyms (country nation state republic commonwealth kingdom superpower))
        (plural (empires))
       )
)
(mcon 'employ
      '((synonyms (apply avail benefit-from bring-into-action bring-to-bear busy engage enjoy enlist exercise exert harness have hire impose improve involve make-use-of occupy put-into-effect put-to-use put-to-work recruit retain spend take-on use use-pro

fitably utilize wield))
	(tenses (employs employed employing))
       )
)
(mcon 'encourage
      '((synonyms (abet activate advance animate approve assist assure authorize back cheer cheer-up comfort commend confirm countenance cultivate develop embolden enliven establish exhort favor fortify foster further hearten help impart-zest-to inspire i

nspirit invigorate invite liven maintain nerve permit promote raise reassure sanction smile-on steel stimulate strengthen support uphold urge-on vitalize welcome))
	(tenses (encourages encouraged encouraging))
       )
)
(mcon 'end
      '((synonyms (abolish cease stop discontinue terminate finish
		   desist quit conclude revoke))
	(tenses (ends ended ending))
       )
)
(mcon 'enemy
      '((superordinates (adversary competitor opponent rival))
	(synonyms (foe opposition))
	(plural (enemies))
       )
)
(mcon 'enemy-of
      '((superordinates (adversary-of competitor-of opponent-of rival-of))
	(synonyms (enemy foe-of))
	(plural (enemies-of))
       )
)
(mcon 'engross
      '((synonyms (absorb concern consume interest occupy))
	(tenses (engrosses engrossed engrossing))
       )
)
(mcon 'enough
      '((synonyms (adequate due sufficient))
	(antonyms (inadequate insufficient meager meagre))
       )
)
(mcon 'enraged
      '((synonyms (angered angry exasperated furious heated irate ireful mad raging wrathful wroth))
	(antonyms (passionless placated placid pleased unemotional))
       )
)
(mcon 'ensign
      '((superordinates (officer naval-officer military-personnel))
	(plural (ensigns))
       )
)
(mcon 'entangle
      '((synonyms (enmesh ensnare ensnarl fetter hold restrain snare tangle trap))
	(tenses (entangles entangled entangling))
       )
)
(mcon 'enter
      '((antonyms (exit leave depart retreat))
	(tenses (enters entered entering))
       )
)
(mcon 'envy
      '((synonyms (begrudge))
	(tenses (envies envied envying))
       )
)
(mcon 'escape
      '((synonyms (avoid elude evade get-away))
	(tenses (escapes escaped escaping))
       )
)
(mcon 'escape-from
      '((synonyms (escape evade get-away-from flee-from))
	(tenses (escapes-from escaped-from escaping-from))
       )
)
(mcon 'estate
      '((synonyms (house-and-grounds homestead lands house household property properties))
	(plural (estates))
       )
)
(mcon 'even-though
      '((synonyms (still although though))
       )
)
(mcon 'evil
      '((synonyms (bad foul heinous malevolent malicious nefarious vile))
	(antonyms (benevolent benign good virtuous))
       )
)
(mcon 'exaggerate
      '((superordinates (lie misinform))
	(synonyms (overstate stretch hyperbolize overemphasize
		   magnify distort))
	(tenses (exaggerates exaggerated exaggerating))
       )
)
(mcon 'excessive
      '((synonyms (excess inordinate lavish profuse redundant superfluous surplus undue unwarranted))
	(antonyms (moderate normal reasonable scanty sparse))
       )
)
(mcon 'excessive-for
      '((synonyms (excessive inordinate-for superfluous-for unwarranted-for))
	(antonyms (reasonable-for scanty-for insufficient-for))
       )
)
(mcon 'exchange
      '((synonyms (barter change convert displace interchange replace shift substitute swap switch trade transfer))
	(tenses (exchanges exchanged exchanging))
       )
)
(mcon 'excuse
      '((synonyms (exemption justification pardon release))
	(plural (excuses))
       )
)
(mcon 'excuse-for
      '((synonyms (excuse exemption-for justification-for pardon-for))
	(plural (excuses-for))
       )
)
(mcon 'executed 
      '((synonyms (killed slain put-to-death dead))
	(subordinates (electrocuted beheaded decapitated guillotined shot hung hanged))
       )
)
(mcon 'exhortation
      '((synonyms (inducement persuasion coaxing cajolery admonition))
	(antonyms (caution warning dissuasion))
	(plural (exhortations))
       )
)
(mcon 'exist
      '((synonyms (live be))
	(tenses (exists existed existing))
       )
)
(mcon 'exit
      '((synonyms (leave depart quit withdraw retreat))
	(tenses (exits exited exiting))
       )
)
(mcon 'expect
      '((synonyms (anticipate await count-on hope plan-on presume rely-on think))
	(tenses (expects expected expecting))
       )
)
(mcon 'expectation
      '((synonyms (anticipation hope prospect))
	(antonyms (inexpectation unanticipation surprise astonishment))
	(plural (expectations))
       )
)
(mcon 'expensive
      '((synonyms (dear high-priced costly valuable))
	(antonyms (inexpensive cheap valueless))
       )
)       ; invaluable as an antonym? (ha-ha-ha :-})
(mcon 'experience
      '((synonyms (endure enjoy feel go-through live-through see taste undergo))
	(tenses (experiences experienced experiencing))
       )
)
(mcon 'extravagant
      '((synonyms (boastful excessive exorbitant exuberant gratifying immoderate inordinate intemperate lavish lucullan lush luxuriant magnificent opulent pampering pompous pretentious prodigal profuse rich spendthrift stiff sumptuous uneconomic uneconomi

cal unreasonable wasteful))
	(antonyms (austere cheap frugal inexpensive little moderate restrained small temperate thrifty))
       )
)
(mcon 'eye-of
      '((superordinates (pairwise-organ sense-organ))
	(synonyms (eye oculus optic peeper eyeball))
	(sub-parts (eyelid lid protective-fold stye eyeball cornea lens retina lacrimal-gland lacrimal-duct))
       )
)
) ; end of semantics_e

(defun semantics_f ()

(mcon 'fact
      '((synonyms (truth reality actuality axiom postulate datum))
	(antonyms (fancy falsehood untruth))
	(plural (facts))
       )
)
(mcon 'fail
      '((synonyms (blunder botch err))
	(antonyms (pass succeed))
	(tenses (fails failed failing))
       )
)
(mcon 'fail-to
      '((synonyms (fail forget-to omit))
	(antonyms (remember-to do))
	(tenses (fails-to failed-to failing-to))
       )
)
(mcon 'faint
      '((synonyms (dim dizzy dull faded fearful feeble giddy indistinct lose-consciousness low pale queasy quiet slight soft swoon swooning timorous vertiginous wan washed-out weak))
	(antonyms (audacious bright clear clear-headed colorful colourful dark distinct loud obvious robust strong))
	(tenses (faints fainted fainting))
       )
)
(mcon 'fair
      '((synonyms (acceptable equitable evenhanded impartial just justified reasonable))
	(antonyms (dishonest unfair unjust unreasonable wrong))
       )
)
(mcon 'fairie
      '((superordinates (supernatural supernatural-being))
	(synonyms (fairy))
       )
)
(mcon 'fall
      '((synonyms (descend drop plunge precipitate topple tumble tumble-down))
	(tenses (falls fell falling))
       )
)
(mcon 'fall-from
      '((synonyms (fall drop-from plunge-from tumble-from))
	(tenses (falls-from fell-from falling-from))
       )
)
(mcon 'fall-in
      '((synonyms (fall fall-into drop-into tumble-into))
	(tenses (falls-in fell-in falling-in))
       )
)
(mcon 'family
      '((synonyms (household related kin))
	(sub-parts (person people))
	(plural (families))
       )
)
(mcon 'family-of
      '((synonyms (family relations-of kin-of))
	(plural (families-of))
       )
)
(mcon 'fancy
      '((synonyms (dressy showy sophistocated special))
	(antonyms (plain sober))
       )
)
(mcon 'farmer
      '((superordinates (worker working-person))
	(plural (farmers))
       )
)
(mcon 'fast
      '((synonyms (diet go-hungry swift speedy rapid quick fleet hasty expeditious agile nimble lively))
	(antonyms (slow leisurely gentle easy unhurried relaxed gradual creeping crawling sluggish languid slothful))
       )
)
(mcon 'faster
      '((superordinates (more fast))
	(synonyms (swifter speedier quicker hastier livelier))
	(antonyms (slower gentler))
       )
)
(mcon 'faster-than
      '((synonyms (faster swifter-than speedier-than quicker-than hastier-than livelier-than))
	(antonyms (slower-than gentler-than))
       )
)
(mcon 'fasten
      '((synonyms (anchor attach bind connect couple glue hitch join link tether tie unite))
	(antonyms (detach disconnect uncouple unhitch untie))
	(tenses (fastens fastened fastening))
       )
)
(mcon 'fasten-to
      '((synonyms (fasten attach-to bind-to connect-to couple-to glue-to hitch-to join-to link-to tether-to tie-to unite-with))
	(antonyms (detach-from disconnect-from uncouple-from unhitch-from untie-from))
	(tenses (fastens-to fastened-to fastening-to))
       )
)
(mcon 'father
      '((superordinates (relative kin parent))
	(plural (fathers))
       )
)
(mcon 'father-of
      '((superordinates (relative-of parent-of))
	(synonyms (father))
	(plural (fathers-of))
       )
)
(mcon 'fault
      '((synonyms (mistake error responsibility))
	(plural (faults))
       )
)
(mcon 'fault-in
      '((synonyms (fault mistake-in error-in))
	(plural (faults-in))
       )
)
(mcon 'fault-of
      '((synonyms (fault responsibility-of caused-by))
	(plural (faults-of))
       )
)
(mcon 'favor
      '((superordinates (diversion))
	(synonyms (aid approval assistance help support))
	(antonyms (disfavor))
	(plural (favors))
       )
)
(mcon 'favor-of
      '((synonyms (favor approval-of liking-of support-of))
	(antonyms (disfavor-of))
       )
)
(mcon 'favor-to
      '((synonyms (favor aid-to assistance-to support-to))
	(plural (favors-to))
       )
)
(mcon 'fawn
      '((superordinates (deer))
	(plural (fawns))
       )
)
(mcon 'fear
      '((synonyms (apprehend dread))
	(tenses (fears feared fearing))
       )
)
(mcon 'fearful
      '((synonyms (afraid apprehensive dreadful scared terrible terrified timorous))
	(antonyms (audacious brave confident fearless unafraid))
       )
)
(mcon 'feast
      '((superordinates (meal repast))
	(synonyms (banquet festival festivity fete))
	(plural (feasts))
       )
)
(mcon 'feather
      '((synonyms (plume quill))
	(part-of (bird down))
	(plural (feathers))
       )
)
(mcon 'feather-of
      '((synonyms (feather plume-of))
	(plural (feathers-of))
       )
)
(mcon 'feathers
      '((synonyms (down fluff))
       )
)
(mcon 'fee
      '((superordinates (money))
	(synonyms (cost expense toll charge charges fare))
	(plural (fees))
       )
)
; OOPS!  I used this inconsistently!
(mcon 'feed
      '((synonyms (give-dinner nourish))
	(tenses (feeds fed feeding))
       )
)
(mcon 'fetch
      '((synonyms (bring convey get))
	(tenses (fetches fetched fetching))
       )
)
(mcon 'feud
      '((synonyms (quarrel dispute argument fight vendetta animosity))
	(antonyms (peace))
	(plural (feuds))
       )
)
(mcon 'feud-between
      '((synonyms (feud quarrel-between argument-between animosity-between))
	(antonyms (peace-between))
	(plural (feuds-between))
       )
)
(mcon 'field
      '((subordinates (court football-field gridiron stadium athletic-field playing-field))
	(synonyms (grassland meadow pasture))
	(part-of (stadium))
	(plural (fields))
       )
)
(mcon 'field-of
      '((synonyms (field meadow-of pasture-of))
	(plural (fields-of))
       )
)
(mcon 'fig
      '((superordinates (fruit))
	(synonyms (caprifig))
	(plural (figs))
       )
)
(mcon 'fight
      '((synonyms (battle struggle combat argument))
	(antonyms (peace))
	(plural (fights))
       )
)
(mcon 'fight-for
      '((synonyms (fight argue-over compete-for))
	(tenses (fights-for fought-for fighting-for))
       )
)
(mcon 'fight-with
      '((synonyms (fight battle-with spar-with wrestle-with argue-with struggle-against))
	(tenses (fights-with fought-with fighting-with))
       )
)
(mcon 'filament
      '((synonyms (strand fiber fibril element))
       )
)
(mcon 'file
      '((superordinates (hand-tool))
	(subordinates (nail-file rasp wood-file))
	(plural (files))
       )
)
(mcon 'filled
      '((synonyms (overflowing swollen))
	(antonyms (empty unoccupied))
       )
)
(mcon 'filled-with
      '((synonyms (filled overflowing-with swollen-with))
	(antonyms (empty-of unoccupied-by))
       )
)
(mcon 'find
      '((synonyms (discover detect locate spot encounter))
	(antonyms (lose miss ignore))
	(tenses (finds found finding))
       )
)
(mcon 'find-in
      '((synonyms (find discover-in detect-in))
	(tenses (finds-in found-in finding-in))
       )
)
(mcon 'fisherman
      '((synonyms (fisher angler troller whaler))
	(plural (fishermen))
       )
)
(mcon 'fit
      '((synonyms (epileptic-fit spasm))
	(plural (fits))
       )
)
(mcon 'fix
      '((synonyms (repair mend restore secure fasten))
	(antonyms (break))
       )
)
(mcon 'flagon
      '((superordinates (vessel))
	(plural (flagons))
       )
)
(mcon 'flattery
      '((synonyms (adulation fawning palaver compliment))
	(antonyms (detraction disapproval))
       )
)
(mcon 'flee
      '((synonyms (depart evade fly run run-away))
	(tenses (flees fled fleeing))
       )
)
(mcon 'flee-from
      '((synonyms (avoid escape-from evade fly-from run-from run-away-from))
	(tenses (flees-from fled-from fleeing-from))
       )
)
(mcon 'fleece
      '((superordinates (flesh hide skin))
	(synonyms (fur wool))
       )
)
(mcon 'fleece-of
      '((superordinates (flesh-of hide-of skin-of))
	(synonyms (fleect fur-of wool-of))
       )
)
(mcon 'float-on
      '((synonyms (float lie-in rest-on ride-on))
	(tenses (floats floated floating))
       )
)
(mcon 'flock
      '((subordinates (pride sloth skulk gang kennel clowder pod gam school covey bevy skein gaggle watch charm hive plague))
	(synonyms (gathering congregation bunch pack colony herd drove swarm))
	(plural (flocks))
       )
)
(mcon 'flock-of
      '((subordinates (pride-of sloth-of skulk-of gang-of kennel-of clowder-of pod-of gam-of school-of covey-of bevy-of skein-of gaggle-of watch-of charm-of hive-of))
	(synonyms (flock gathering-of congregation-of bunch-of pack-of colony-of herd-of drove-of swarm-of))
	(plural (flocks-of))
       )
)
(mcon 'flute
      '((superordinates (woodwind woodwind-instrument))
	(subordinates (ocarina piccolo recorder whistle))
	(plural (flutes))
       )
)
(mcon 'flutter-to
      '((superordinates (move-to go-to))
	(synonyms (flutter))
	(tenses (flutters-to fluttered-to fluttering-to))
       )
)
(mcon 'fly
      '((synonyms (glide move-through-air sail soar))
	(tenses (flies flew flying))
       )
)
(mcon 'fly-at
      '((synonyms (fly charge hit rush rush-against rush-at strike))
	(tenses (flies-at flew-at flying-at))
       )
)
(mcon 'fly-to
      '((superordinates (move-to go-to))
	(synonyms (fly soar-to glide-to sail-to))
	(tenses (flies-to flew-to flying-to))
       )
)
(mcon 'fold
      '((superordinates (enclosure))
	(synonyms (pen sheepfold))
	(plural (folds))
       )
)
(mcon 'follow
      '((synonyms (pursue obey comply observe))
	(tenses (follows followed following))
       )
)
(mcon 'follower
      '((synonyms (ally friend crony henchman))
	(plural (followers))
       )
)
(mcon 'food
      '((superordinates (object))
	(subordinates (aliment beverage bread drink egg fowl grain milk meal meat nutriment produce))
	(synonyms (nutriment))
	(plural (foods))
       )
)
(mcon 'fool
      '((superordinates (unfortunate-person))
	(synonyms (blockhead chump clod cretin dimwit dummy dunce idiot oaf simpleton))
	(plural (fools))
       )
)
(mcon 'foolish
      '((synonyms (absurd fantastic harebrained ill-advised impractical imprudent inane irrational ludicrous ridiculous senseless silly unwise))
	(antonyms (clever intelligent practical realistic reasonable sane wise))
       )
)
(mcon 'forbid
      '((synonyms (ban bar block debar disallow enjoin exclude hinder interdict negative obstruct obviate outlaw preclude prevent prohibit proscribe stop veto warn-off))
	(tenses (forbids forbid forbidding))
       )
)
(mcon 'force
      '((synonyms (drive effect leverage power strength))
	(plural (forces))
       )
)
(mcon 'force-to
      '((synonyms (force coerce-to compel-to drive-to impel-to motivate-to press-to push-to))
	(tenses (forces-to forced-to forcing-to))
       )
)
(mcon 'foreign
      '((synonyms (alien nonnative strange unfamiliar))
	(antonyms (familiar native relevant))
       )
)
(mcon 'foreign-to
      '((synonyms (foreign alien-to nonnative-to))
	(antonyms (native-to relevant-to))
       )
)
(mcon 'forest
      '((synonyms (wood woodland timberland timber woods jungle))
	(subordinates (forest-preserve state-forest national-forest
		       rain-forest cloud-forest climax-forest sprout-forest
		       selection-forest primeval-forest virgin-forest))
       )
)
(mcon 'forethought
      '((synonyms (foresight premeditation anticipation farsightedness precaution))
	(antonyms (rashness unwariness carelessness impetuousness daring))
       )
)
(mcon 'forget
      '((synonyms (disregard neglect omit overlook slight))
	(antonyms (remember))
	(tenses (forgets forgot forgetting))
       )
)
(mcon 'forget-how
      '((synonyms (forget))
	(antonyms (remember-how))
	(tenses (forgets-how forgot-how forgetting-how))
       )
)
(mcon 'forget-that
      '((synonyms (forget neglect overlook))
	(antonyms (remember-that remember))
       )
)
(mcon 'forgive
      '((synonyms (pardon excuse countenance absolve acquit))
	(tenses (forgives forgave forgiving))
       )
)
(mcon 'forsee
      '((synonyms (anticipate forestall predict foretell soothsay forecast prophesy))
	(tenses (forsees foresaw forseeing))
       )
)
(mcon 'forswear
      '((synonyms (abjure disclaim give-up quit recant reject relinquish renounce resign retract))
	(tenses (forswears forswore forswearing))
       )
)
(mcon 'fortress
      '((synonyms (bastion citadel fort fortification keep))
	(superordinates (stronghold hold))
       )
)
(mcon 'fox
      '((superordinates (canid mammal))
	(plural (foxes))
       )
)
(mcon 'frankincense
      '((superordinates (gum resin))
	(synonyms (olibanum))
       )
)
(mcon 'free
      '((synonyms (unconfined unconstrained loose))
	(antonyms (bound confined obstructed closed restricted))
       )
)
(mcon 'friend
      '((superordinates (associate))
	(synonyms (ally confederate buddy chum companion confidant crony pal sidekick))
	(antonyms (enemy foe))
	(plural (friends))
       )
)
(mcon 'friendless
      '((synonyms (lonely lonesome hermit solitary))
	(antonyms (gregarious))
       )
)
(mcon 'friend-of
      '((synonyms (friend buddy-of chum-of companion-of pal-of))
	(antonyms (enemy-of))
	(plural (friends-of))
       )
)
(mcon 'friendly
      '((synonyms (affable amiable amicable congenial cordial genial good-humored gracious peaceable peaceful pleasant sociable warmhearted))
	(antonyms (hateful hostile ill-natured malevolent solitary unfriendly unkind unpleasant unsociable))
       )
)
(mcon 'frighten
      '((synonyms (alarm scare dismay daunt intimidate dishearten terrorize terrify affright))
	(antonyms (pacify quiet calm soothe))
	(tenses (frightens frightened frightening))
       )
)
(mcon 'frightened
      '((synonyms (afraid aghast alarmed fearful panicky scared terrified))
	(antonyms (fearless unafraid))
       )
)
(mcon 'frightening
      '((synonyms (dread fearsome frightful nightmarish scary terrible terrifying))
	(antonyms (reassuring))
       )
)
(mcon 'frog
      '((superordinates (amphibian))
	(subordinates (south-american-poison-toad surinam-toad tree-toad))
	(synonyms (toad))
	(plural (frogs))
       )
)
(mcon 'frozen
      '((synonyms (benumbed cold numb unthawed))
	(antonyms (hot melted))
       )
)
(mcon 'fruit
      '((superordinates (food))
	(subordinates (apple bananna berry fig grape orange peach pear plum))
	(sub-parts (seed juice peel))
	(part-of (plant tree))
       )
)
(mcon 'function
      '((synonyms (act do execute perform behave work operate run))
	(antonyms (misfunction dysfunction not-function))
	(tenses (functions functioned functioning))
       )
)
(mcon 'funds
      '((synonyms (finances assets pecuniary-resources means wherewithal budget treasure resources capital))
       )
)
(mcon 'fuse
      '((synonyms (amalgamate combine fuze join melt melt-together merge unify unite))
	(tenses (fuses fused fusing))
       )
)
(mcon 'fused
      '((synonyms (melted-together blended integrated merged melted
		   united coalesced joined unified amalgamated))
       )
)
) ; end of semantics_f

(defun semantics_g ()

(mcon 'gain
      '((synonyms (acquire attain bag derive get obtain receive win))
	(tenses (gains gained gaining))
       )
)
(mcon 'game
      '((superordinates (animal fauna passtime))
	(synonyms (prey quarry sport competition))
	(plural (game games))
      )
)
(mcon 'gang
      '((superordinates (group))
	(synonyms (crew staff personnel criminals cronies friends crowd street-gang))
	(sub-parts (person people))
	(plural (gangs))
       )
)
(mcon 'general
      '((superordinates (leader military-personnel))
	(synonyms (commander leader admiral))
	(plural (generals))
       )
)
(mcon 'generous
      '((synonyms (abundant ample beneficent benevolent big-hearted charitable giving good-hearted kindly large lavish liberal plentiful prodigal sharing substantial ungrudging wholehearted))
	(antonyms (coldhearted envious little malevolent meager meagre scanty selfish small sparse stingy uncharitable ungenerous unkind))
       )
)
(mcon 'gentle
      '((synonyms (aristocratic balmy benign blue-blooded chivalric chivalrous clement compassionate concerned considerate coroneted courteous delicate docile domesticate easy fair feeling fond forbearing gallant genial gracious highborn humane hushed ing

ratiating inoffensive kind kindly leisurely lenient lovable loving low meek mellow mild modulated patrician pleasant responsive restful sensitive smooth soft soft-spoken soothing suave submissive tame tender tenderhearted titled tractable train understand

ing unhurried upper-class warm warmhearted weak well-bred wellborn))
	(antonyms (abrupt defiant fast hard heavy hostile ignoble impolite inclement inhumane insensitive intense loud malevolent malign merciless noisy pungent rough sour strong tough uneven ungracious unkind unpleasant unquiet unsympathetic wild))
       )
)
(mcon 'gentleman
      '((superordinates (aristocrat))
	(synonyms (man))
	(plural (gentlemen))
       )
)
(mcon 'get
      '((synonyms (acquire catch obtain receive secure seize snatch win))
	(tenses (gets got getting))
       )
)
(mcon 'get-from
      '((synonyms (get acquire-from obtain-from take-from accept-from receive-from))
	(tenses (gets-from got-from getting-from))
       )
)
(mcon 'get-involved-in
      '((synonyms (entangle become-entangled-in become-involved))
	(antonyms (avoid ignore))
       )
)
(mcon 'gift
      '((synonyms (gift present offering award gratuity charity))
	(plural (gifts))
       )
)
(mcon 'gifted
      '((synonyms (bright intelligent talented))
	(antonyms (retarded))
       )
)
(mcon 'girl
      '((superordinates (child female human youngster))
	(synonyms (lass maid maiden young-girl young-lady young-woman))
	(plural (girls))
       )
)
(mcon 'give
      '((synonyms (bestow confer dispense present proffer render))
	(antonyms (accept acquire appropriate gain get obtain
		   receive take))
	(tenses (gives gave giving))
       )
)
(mcon 'give-birth-to
      '((synonyms (bear give-birth bring-forth breed deliver beget))
	)
)
(mcon 'give-to
      '((synonyms (give present-to proffer-to pass-to))
	(antonyms (take take-from accept-from get-from obtain-from receive-from))
	(tenses (gives-to gave-to giving-to))
       )
)
(mcon 'glass
      '((superordinates (substance transparent-substance))
	(sub-parts (silicon))
       )
)
(mcon 'glide-to
      '((superordinates (move-to go-to))
	(synonyms (glide fly-to soar-to sail-to drift-to slide-to))
	(tenses (glides-to glided-to gliding-to))
       )
)
(mcon 'gnat
      '((superordinates (insect))
	(plural (gnats))
       )
)
(mcon 'go
      '((synonyms (betake-oneself betake-oneself-to be-in-motion come come-along continue depart draw-away draw-toward frequent get-along glide-away go-on leave move quit range reach repair-to resort-to roll-by run slide-by slip-away slip-by travel withdr

aw))
	(tenses (goes went going))
       )
)
(mcon 'go-away
      '((synonyms (go depart leave retire vanish withdraw))
	(tenses (goes-away went-away going-away))
       )
)
(mcon 'go-for
      '((synonyms (go fly-at rush-at rush-to))
	(tenses (goes-for went-for going-for))
       )
)
(mcon 'go-to
      '((synonyms (go move-to))
	(tenses (goes-to went-to going-to))
       )
)
(mcon 'goal
      '((synonyms (intention objective aim ambition))
	(plural (goals))
       )
)
(mcon 'goat
      '((superordinates (bovid))
	(subordinates (billy kid nanny))
	(plural (goats))
       )
)
(mcon 'goatherd
      '((superordinates (herdsman))
	(plural (goatherds))
       )
)
(mcon 'god
      '((synonyms (deity divinity goddess))
	(plural (gods))
       )
)
(mcon 'goddess
      '((synonyms (deity divinity god))
	(plural (goddesses))
       )
)
(mcon 'gold
      '((superordinates (metal-element money))
       )
)
(mcon 'good
      '((synonyms (dutiful ethical honest lawful moral obedient pure
			   right upright virtuous wholesome))
	(antonyms (bad disobedient evil immoral wrong wicked))
       )
)
(mcon 'government
      '((synonyms (civic civil governmental municipal))
	(antonyms (private))
	(plural (governments))
       )
)
(mcon 'governor
      '((superordinates (official public-servant))
	(subordinates (chief-of-state crowned-head emperor head-of-state king monarch prince sovereign))
	(synonyms (lord master ruler))
	(plural (governors))
       )
)
(mcon 'governor-of
      '((superordinates (official public-servant))
	(subordinates (chief-of-state crowned-head emperor-of head-of-state king-of monarch-of prince-of sovereign-of governor))
	(synonyms (lord master ruler))
	(plural (governors-of))
       )
)
(mcon 'grace
      '((synonyms (blessing))
       )
)
(mcon 'grape
      '((superordinates (fruit))
	(subordinates (currant raisin))
	(plural (grapes))
       )
)
(mcon 'grass
      '((superordinates (plant))
	(synonyms (pasture grain meadow pasture range prairie marijuana hay pot))
	(plural (grasses))
       )
)
(mcon 'grateful-to
      '((synonyms (grateful appreciative-of thankful-to))
	(antonyms (ungrateful-to))
       )
)
(mcon 'grave
      '((superordinates (burial-chamber))
	(subordinates (sepulcher tomb catacomb))
	(sub-parts (gravestone headstone tombstone))
	(part-of (graveyard))
	(plural (graves))
       )
)
(mcon 'grave-of
      '((subordinates (sepulcher-of tomb-of catacomb-of))
	(sub-parts (gravestone headstone tombstone))
	(part-of (graveyard))
	(plural (graves-of))
       )
)
(mcon 'graze
      '((synonyms (eat feed feed-on))
	(tenses (grazes grazed grazing))
       )
)
(mcon 'great
      '((synonyms (broad capacious considerable epic extensive extreme important large major notable noteworthy outstanding prominent substantial wide-ranging))
	(antonyms (incomplete inferior little ordinary small unimportant))
       )
)
(mcon 'greater
      '((superordinates (more great))
	(synonyms (higher larger superior))
	(antonyms (inferior lesser smaller))
       )
)
(mcon 'grieve
      '((synonyms (afflict cause-grief curse distress feel-grief harass hurt injure mourn oppress persecute punish sadden sorrow try))
	(tenses (grieves grieved grieving))
       )
)
(mcon 'ground
      '((superordinates (material matter substance))
	(subordinates (adobe clay dust humus loam mud sand silt sod truf))
	(synonyms (dirt earth floor land soil))
	(plural (grounds))
       )
)
(mcon 'group
      '((superordinates (social-category))
	(subordinates (aides assistants crew employees gang help staff))
	(synonyms (bunch gang class cluster collection collective concentration congregation cooperative gathering institution type))
	(antonyms (individual separate))
	(plural (groups))
       )
)
(mcon 'grow
      '((synonyms (become-greater bud develop emerge enlarge expand extend flourish germinate mature multiply spread sprout swell wax))
	(tenses (grows grew growing))
       )
)
(mcon 'grow-up
      '((synonyms (grow develop get-older mature))
	(tenses (grows-up grew-up growing-up))
       )
)
(mcon 'guard
      '((synonyms (defend shield protect preserve))
	(tenses (guards guarded guarding))
       )
)
(mcon 'guest
      '((synonyms (visitor caller company))
	(plural (guests))
       )
)
(mcon 'guide
      '((synonyms (direct aim guide pilot lead instruct navigate steer control))
	(antonyms (misdirect mislead))
	(tenses (guides guided guiding))
       )
)
(mcon 'guild
      '((synonyms (fellowship society brotherhood fraternity fraternal-order sisterhood sorority club))
       )
)
) ; end of semantics_g

(defun semantics_h ()

(mcon 'halcyon
      '((superordinates (bird))
	(synonyms (kingfisher))
	(plural (halcyons))
       )
)
(mcon 'half
      '((synonyms (part fraction subset))
	(plural (halves))
       )
)
(mcon 'handicap
      '((synonyms (disability hinderance interference problem))
	(plural (handicaps))
       )
)
(mcon 'handicap-for
      '((synonyms (handicap disability-for hinderance-to problem-for))
	(plural (handicaps-for))
       )
)
(mcon 'handsome
      '((synonyms (beautiful good-looking))
	(antonyms (ugly))
       )
)
(mcon 'hang
      '((synonyms (dangle execute suspend))
	(tenses (hangs hanged hung hanging))
       )
)
(mcon 'hard
      '((synonyms (arduous concrete demanding effortful firm grueling hardened harsh inclement laborious rugged severe strenuous toilsome tough))
	(antonyms (easy soft tender weak))
       )
)
(mcon 'harder
      '((synonyms (firmer harsher severer tougher))
	(antonyms (easier softer weaker))
       )
)
(mcon 'hare
      '((superordinates (lagomorph mammal))
	(synonyms (jackrabbit))
	(plural (hares))
       )
)
(mcon 'harm
      '((synonyms (abuse injure hurt insult wound wrong))
	(antonyms (help aid))
	(tenses (harms harmed harming))
       )
)
(mcon 'harmless
      '((synonyms (innocent innocuous safe unthreatening))
	(antonyms (harmful lethal noxious painful toxic unsafe))
       )
)
(mcon 'hatch
      '((synonyms (bear beget breed conceive engender have procreate produce sire))
	(tenses (hatches hatched hatching))
       )
)
(mcon 'hatch-into
      '((synonyms (hatch become grow-into mature-into))
	(tenses (hatches-into hatched-into hatching-into))
       )
)
(mcon 'hate
      '((synonyms (abhor loathe detest abominate execrate dislike))
	(antonyms (like love accept))
	(tenses (hates hated hating))
       )
)
(mcon 'hateful
      '((synonyms (abhorrent abominable appalling contemptible detestable disgusting horrid malicious mean nasty odious offensive unspeakable))
	(antonyms (decent inoffensive kind lovable))
       )
)
(mcon 'hateful-to
      '((synonyms (hateful appalling-to contemptible-to disgusting-to))
	(antonyms (acceptable-to liked-by loved-by))
       )
)
(mcon 'have
      '((synonyms (hold own possess))
	(tenses (has had having))
       )
)
(mcon 'hawk
      '((superordinates (bird))
	(subordinates (buzzard falcon kite))
	(plural (hawks))
       )
)
(mcon 'head-of
      '((superordinates (bodily-appendage))
	(synonyms (attic bean noodle noggin))
	(sub-parts (jaw muzzle mouth nose proboscis snout ear flap lug organ_of_hearing_and_equilibrium brain grey_matter pharynx pate crown scalp skull cranium face clock dial frontage kisser smiler visage countenance mug temple crown pate face hair))
       )
)
(mcon 'headband
      '((synonyms (headdress headware kerchief handkerchief bandanna))
	(plural (headbands))
       )
)
(mcon 'healthy
      '((synonyms (able-bodied fit hale robust sane sound normal whole))
	(antonyms (abnormal insane unfit unhealthy unsound))
       )
)
(mcon 'hear
      '((synonyms (find-out learn perceive understand))
	(tenses (hears heard hearing))
       )
)
(mcon 'hear-that
      '((synonyms (hear learn-that discover-that))
	(tenses (hears-that heard-that hearing-that))
       )
)
(mcon 'heaven
      '((subordinates (nirvana olympus elysium avalon valhalla))
	(synonyms (paradise glory eternity hereafter afterworld afterlife))
	(plural (heavens)) ; Uhhh... My Heavens?  Naah...
       )
)
(mcon 'hedgehog
      '((superordinates (insectivore mammal))
	(plural (hedgehogs))
       )
)
(mcon 'help
      '((synonyms (aid assist relieve support))
	(tenses (helps helped helping))
       )
)
(mcon 'help-with
      '((synonyms (help aid-with assist-with))
	(tense (helps-with helped-with helping-with))
       )
)
(mcon 'hen
      '((superordinates (chicken bird fowl poultry))
	(plural (hens))
       )
)
(mcon 'herdsman
      '((superordinates (working-person animal-tending-person))
	(subordinates (goatherd shepherd cowherd cowboy swineherd))
	(synonynms (herder herdboy))
	(plural (herdsmen))
       )
)
(mcon 'hermit
      '((superordinates (unfortunate-person unpleasant-person))
	(synonyms (anchorite eremite recluse))
	(plural (hermits))
       )
)
(mcon 'heron
      '((superordinates (wading-bird bird))
	(subordinates (bittern egret))
	(plural (herons))
       )
)
; The syntax of this should be "hide (obj-actor obj-hidden)", so fix f81-7
(mcon 'hide
      '((synonyms (bury cache cloak conceal cover disguise ensconce mask obscure screen shield veil))
	(tenses (hides hid hiding))
       )
)
(mcon 'high
      '((synonyms (large-amount-of great-degree-of))
	(antonyms (low))
       )
)
(mcon 'hire
      '((synonyms (charter employ engage enlist))
	(tenses (hires hired hiring))
       )
)
(mcon 'hole
      '((superordinates (opening orifice outlet passage))
	(subordinates (bore bore-hole drill-hole mouse-hole))
	(synonyms (opening perforation tear))
	(part-of (cheese donut doughnut bagel torus))
	(plural (holes))
       )
)
(mcon 'home
      '((superordinates (building edifice refuge sanctuary))
	(synonyms (homestead))
	(plural (homes))
       )
)
(mcon 'home-of
      '((synonyms (home house-of homestead-of))
	(plural (homes-of))
       )
)
(mcon 'homeless
      '((synonyms (displaced dispossessed refugee))
       )
)
(mcon 'honest
      '((synonyms (candid ethical frank genuine good honorable lawful moral sincere straight true truthful))
	(antonyms (crooked dishonest false immoral insincere wrong))
       )
)
(mcon 'honor
      '((synonyms (dignity esteem pride respect reverence))
	(plural (honors))
       )
)
; OOPS!  Used this inconsistently.  Add hope-that
(mcon 'hope
      '((superordinates (expectation))
	(synonyms (belief desire expectations trust))
	(plural (hopes))
       )
)
(mcon 'horn
      '((superordinates (bone instrument))
	(subordinates (bugle cornet french-horn hunting-horn trumpet))
	(synonyms (antler))
	(part-of (bull rhinoceros ungulate))
	(plural (horns))
       )
)
(mcon 'horn-of
      '((superordinates (bone-of))
	(synonyms (horn antler-of))
	(plural (horns-of))
       )
)
(mcon 'hope-that
      '((synonyms (hope believe-that desire-that expect-that trust-that))
	(tenses (hopes-that hoped-that hoping-that))
       )
)
(mcon 'horse
      '((superordinates (equid equine mammal))
	(subordinates (arabian bronco buck cart-horse coach-horse colt draft-horse filly foal gelding mare morgan mustang nag palomino pinto pony quarter-horse race-horse stallion steed stud))
	(plural (horses))
       )
)
(mcon 'house
      '((superordinates (dwelling))
	(subordinates (bungalow cabin cottage farmhouse manor))
	(sub-parts (attic dormer garret loft))
	(plural (houses))
       )
)
(mcon 'house-of
      '((superordinates (dwelling-of))
	(subordinates (bungalow-of cabin-of cottage-of))
	(sub-parts (attic dormer garret loft))
	(plural (houses-of))
       )
)
(mcon 'huge
      '((synonyms (astronomical colossal enormous galactic immense tremendous vast))
	(antonyms (little small))
       )
)
(mcon 'human
      '((superordinates (mammal))
	(synonyms (person human-being individual))
	(plural (humans))
       )
)
(mcon 'humble
      '((synonyms (common homespun low-class lowly meek modest passive submissive unassuming))
	(antonyms (assuming pretentious proud))
       )
)
(mcon 'hungry
      '((synonyms (famished ravenous voracious))
	(antonyms (fed full))
       )
)
(mcon 'hunter
      '((superordinates (sportsman))
	(synonyms (huntsman killer))
	(plural (hunters))
       )
)
(mcon 'huntsman
      '((synonyms (hunter))
	(plural (huntsmen))
       )
)
(mcon 'hurt
      '((synonyms (abuse afflict harass harm injure maltreat mistreat pain persecute wound wrong))
	(tenses (hurts hurt hurting))
       )
)
(mcon 'husband
      '((superordinates (spouse person man relation kin))
	(synonyms (married-man))
	(plural (husbands))
       )
)
) ; end of semantics_h

