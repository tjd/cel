
(defun semantics_i ()

(mcon 'if
      '((synonyms (supposing supposing-that assuming-that))
       )
)
(mcon 'ignorant
      '((synonyms (benighted heathen illiterate uneducated uninformed untaught untrained untutored unversed))
	(antonyms (aware educated informed literate skilled))
       )
)
(mcon 'ignorant-of
      '((synonyms (ignorant unaware-that uninformed-about uninformed-of))
	(antonyms (aware-that informed-about informed-of))
       )
)
(mcon 'ignore
      '((synonyms (disregard forget neglect overlook))
	(antonyms (attend observe remember))
	(tenses (ignores ignoring ignored))
       )
)
(mcon 'ill
      '((synonyms (infirm sick unwell unhealthy))
	(antonyms (healthy well))
       )
)
(mcon 'ill-natured
      '((synonyms (irritable cross cranky testy cantankerous mean))
	(antonyms (good-natured nice even-tempered gentle))
       )
)
(mcon 'imitate
      '((synonyms (copy duplicate emulate mimic mock simulate))
	(tenses (imitates imitated imitating))
       )
)
(mcon 'impatient
      '((synonyms (anxious intolerant irritable))
	(antonyms (patient tolerant))
       )
)
(mcon 'impatient-with
      '((synonyms (impatient intolerant-of bored-with))
	(antonyms (patient-with tolerant-of))
       )
)
(mcon 'imperil
      '((synonyms (endanger jeopardize expose comprimise))
	(antonyms (protect guard safeguard secure shelter))
	(tenses (imperils imperiled imperilled imperiling imperilling))
       )
)
(mcon 'imply
      '((synonyms (connote entail implicate indicate insinuate suggest))
	(tenses (implys implyed implying))
       )
)
(mcon 'impossible
      '((synonyms (insoluble unachievable))
	(antonyms (possible))
       )
)
(mcon 'impostor
      '((synonyms (imitator ringer pertender charlatan impersonator mimic))
	(antonyms (original real))
	(plural (impostors))
       )
)
(mcon 'impoverish
      '((synonyms (bankrupt break deplete depress distress ruin))
	(tenses (impoverishes impoverished impoverishing))
       )
)
(mcon 'impregnate
      '((synonyms (breed mate-with inseminate get-with-child))
	(tenses (impregnates impregnated impregnating))
       )
)
(mcon 'imprison
      '((synonyms (trap confine jail incarcerate hold arrest))
	(tenses (imprisons imprisoned imprisoning))
       )
)
(mcon 'in
      '((synonyms (inside within at inside-of near among by on))
	(antonyms (out outside outside-of))
       )
)
(mcon 'incapable
      '((synonyms (incompetent inept unable unqualified))
	(antonyms (able capable competent qualified))
       )
)
(mcon 'incense
      '((synonyms (frankincense olibanum))
	(plural (incenses))
       )
)
(mcon 'incest
      '((superordinates (immorality sex sexual-intercourse))
       )
)
(mcon 'incestuous
      '((superordinates (sexual immoral))
	(antonyms (moral))
       )
)
(mcon 'indignant
      '((synonyms (annoyed huffy irritated outraged vindictive))
	(antonyms (placated))
       )
)
(mcon 'injure
      '((synonyms (abuse attack bruise damage harass harm hurt insult pain sprain wound wrong))
	(tenses (injures injured injuring))
       )
)
; Used inconsistently. m45, f99
(mcon 'injured
      '((synonyms (afflicted crippled handicapped harmed hurt lame wounded))
	(antonyms (healthy unharmed))
       )
)
(mcon 'inside
      '((synonyms (within))
	(antonyms (without outside))
       )
)
(mcon 'inside-of
      '((synonyms (inside within))
	(antonyms (outside-of outside))
       )
)
(mcon 'insincere
      '((synonyms (affected assumed deceitful deceptive dishonest disingenuous dissembling feigned hollow hypocritical meretricious misleading pretended sham simulated unreal))
	(antonyms (genuine honest natural sincere true))
       )
)
(mcon 'insolvent
      '((synonyms (bankrupt broke busted))
	(antonyms (rich))
       )
)
(mcon 'instinct
      '((subordinates (libido id))
	(synonyms (impulse))
	(plural (instincts))
       )
)
(mcon 'instinct-of
      '((subordinates (libido-of id-of))
	(synonyms (instinct impulse-of))
	(plural (instincts-of))
       )
)
(mcon 'insult
      '((synonyms (offend mock scorn deride taunt))
	(tenses (insults insulted insulting))
       )
)
(mcon 'insulted-by
      '((synonyms (insulted offended-by degraded-by))
       )
)
(mcon 'intelligent
      '((synonyms (bright brilliant clever sharp quick smart wise))
	(antonyms (dim obtuse retarded stupid unintelligent))
       )
)
(mcon 'intend-to
      '((synonyms (aim-to aspire-to expect-to hope-to mean-to plan-to will wish-to))
	(tenses (intends-to intended-to intending-to))
       )
)
(mcon 'intensity
      '((synonyms (power amount strength))
	(plural (intensities))
       )
)
(mcon 'invite
      '((synonyms (ask entreat lure tempt welcome))
	(tenses (invites invited inviting))
       )
)
(mcon 'invite-to
      '((synonyms (invite ask-to suggest-to))
	(tenses (invites-to invited-to inviting-to))
       )
)
(mcon 'island
      '((superordinates (land))
	(antonyms (continental))
	(plural (islands))
       )
)
) ; end of semantics_i

(defun semantics_jk ()

(mcon 'jackdaw
      '((superordinates (bird passerine songbird songster))
	(plural (jackdaws))
       )
)
(mcon 'jailed
      '((synonyms (held imprisoned))
	(antonyms (unconfined))
       )
)
(mcon 'jealous
      '((synonyms (envious))
	(antonyms (unenvious))
       )
)
(mcon 'jealous-of
      '((synonyms (jealous envious-of))
       )
)
(mcon 'job
      '((synonyms (employment work occupation))
	(plural (jobs))
       )
)
(mcon 'join
      '((synonyms (adjoin affix ally amalgamate attach coalesce combine compound conjoin connect couple fasten fuse hitch integrate join-company link merge relate tie travel-with unify unite))
	(tenses (joins joined joining))
       )
)
(mcon 'journey
      '((synonyms (cruise jaunt peregrinate tour travel trip voyage))
	(tenses (journeys journeyed journeying))
       )
)
(mcon 'joyfully
      '((synonyms (joyful cheerful high-spirited joyous lively merry sportive jolly glad))
	(antonyms (joyless dejected unhappy sad low-spirited sorrowful sober))
       )
)
(mcon 'jug
      '((superordinates (vessel))
	(synonyms (pitcher bottle))
	(plural (jugs))
       )
)
(mcon 'juice
      '((superordinates (beverage drink liquid peg thirst-quencher))
	(subordinates (fruit-juice v-8-juice))
	(plural (juices))
       )
)
(mcon 'jump
      '((synonyms (bounce bound hop leap spring vault))
	(tenses (jumps jumped jumping))
       )
)
(mcon 'jump-in
      '((synonyms (jump hop-in leap-in spring-in vault-in))
	(tenses (jumps-in jumped-in jumping-in))
       )
)
(mcon 'jump-out-of
      '((synonyms (jump hop-out-of leap-out-of spring-out-of))
	(tenses (jumps-out-of jumped-out-of jumping-out-of))
       )
)
(mcon 'just
      '((synonyms (equitable fair honorable impartial upright))
	(antonyms (dishonest false unfair unjust unreasonable))
       )
)
(mcon 'justice
      '((synonyms (equity fairness blind-justice))
       )
)
(mcon 'justified
      '((synonyms (valid fair just reasonable right))
	(antonyms (unfair unreasonable wrong))
      )
)
(mcon 'keep
      '((synonyms (have maintain-possession reserve retain save stock store))
	(tenses (keeps kept keeping))
       )
)
(mcon 'keep-in
      '((synonyms (keep hold-in store-in put-in))
	(tenses (keeps-in kept-in keeping-in))
       )
)
(mcon 'kill
      '((synonyms (murder eliminate massacre slaughter slay execute assassinate dispatch shoot butcher))
	(tenses (kills killed killing))
       )
)
(mcon 'killed
      '((synonyms (exterminated eradicated murdered eliminated deadened massacred slaughtered wiped-out slayed assassinated dispatched executed wasted dead))
       )
)
(mcon 'kin
      '((synonyms (relative relation kindred))
	(antonyms (unrelated))
	(plural (kin))
       )
)
(mcon 'kind
      '((synonyms (beneficent benevolent benign compassionate considerate courteous disarming genial gentle gracious helpful kindly lovable loving pleasant sympathetic tender warm))
	(antonyms (austere evil hostile inconsiderate malevolent malign sour uncharitable ungracious unkind unloving))
       )
)
(mcon 'kindest
      '((superordinates (most kind))
	(synonyms (gentlest kindliest mildest tenderest warmest))
	(antonyms (austerest evilest sourest unkindest))
       )
)
(mcon 'kindness
      '((synonyms (beneficence benevolence compassion consideration courtesy geniality gentleness helpfulness indulgence kindliness pleasantness sympathy tenderness tolerance warmness))
	(antonyms (austerity hostility malevolence sourness unkindness))
	(plural (kindnesses))
       )
)
(mcon 'king
      '((superordinates (nobleman aristocrat patrician ruler))
	(synonyms (emperor monarch sovereign))
	(plural (kings))
       )
)
(mcon 'king-of
      '((superordinates (nobleman aristocrat patrician ruler))
	(synonyms (king emperor-of monarch-of sovereign-of))
	(plural (kings-of))
       )
)
(mcon 'kingdom
      '((synonyms (country nation land empire realm dominion domain))
	(plural (kingdoms))
       )
)
(mcon 'kite
      '((superordinates (hawk bird))
	(plural (kites))
       )
)
(mcon 'know
      '((synonyms (perceive apprehend understand see sense comprehend
		   be-aware))
	(tenses (knows knew knowing))
       )
)
(mcon 'know-that
      '((synonyms (know apprehend-that understand-that see-that sense-that comprehend-that be-aware-that))
	(tenses (knows-that knew-that knowing-that))
       )
)
) ; end of semantics_jk

(defun semantics_l ()

(mcon 'labor
      '((synonyms (work toil))
	(plural (labors))
       )
)
(mcon 'laborer 
      '((superordinates (worker))
	(synonyms (manual-laborer unskilled-worker))
	(plural (laborers))
       )
)
(mcon 'lady
      '((superordinates (woman female person aristocrat))
	(subordinates (abbess hussy slut wench baroness countess duchess))
	(synonyms (woman))
	(plural (ladies))
       )
)
(mcon 'lag-behind
      '((synonyms (dally drag fall-behind hang-behind linger procrastinated trail))
	(tenses (lags-behing lagged-behind lagging-behind))
       )
)
(mcon 'lamb
      '((superordinates (sheep child))
	(plural (lambs))
       )
)
(mcon 'land
      '((superordinates (place))
	(synonyms (earth ground terra-firma country))
	(antonyms (sea ocean air sky water))
	(plural (lands))
       )
)
(mcon 'large
      '((synonyms (big bulky capacious great large-scale massive formidable))
	(antonyms (little microscopic narrow short small))
       )
)
(mcon 'larger-than
      '((superordinates (more large))
	(synonyms (greater-than higher-than superior-to))
	(antonyms (inferior-to littler-than smaller-than))
       )
)
(mcon 'lark
      '((superordinates (passerine songbird songster bird))
	(subordinates (meadowlark meadow-lark pipit skylark titlark))
	(plural (larks))
       )
)
(mcon 'laser
      '((superordinates (device tool light-source radiation-source))
	(plural (lasers))
       )
)
(mcon 'laugh
      '((synonyms (chortle chuckle giggle))
	(tenses (laughs laughed laughing))
       )
)
(mcon 'laugh-at
      '((synonyms (laugh chuckle-at giggle-at scorn sneer-at make-fun-of))
	(tenses (laughs-at laughed-at laughing-at))
       )
)
(mcon 'law
      '((synonyms (statute canon ordinance rule regulation))
	(plural (laws))
       )
)
(mcon 'lawyer
      '((superordinates (professional-person))
	(subordinates (judge justice magistrate))
	(synonyms (advocate attorney barrister counsel counselor counselor-at-law solicitor))
	(plural (lawyers))
       )
)
(mcon 'lax
      '((synonyms (relaxed loose negligent unrestrictive))
	(antonyms (diligent restrictive))
       )
)
(mcon 'lay
      '((synonyms (copulate-with make place plain put set stake stow unpriestly unspecialized untechnical venture wager))
	(antonyms (professional sacred technical))
	(plural (lays))
	(tenses (lays layed layying))
       )
)
(mcon 'lead
      '((synonyms (command bear be-first be-in-front bring bring-to carry cause cause-to-come conduct continue convey deliver direct divert draw engineer escort extend go go-ahead guide head induce lure marshal marshall navigate pilot prevail-upon reach r

un show show-in show-the-way steer usher))
	(tenses (leads lead leading))
       )
)
(mcon 'lead-to
      '((synonyms (go-to go-towards point-towards direct-towards
		   conduct conduct-towards conduct-to))
       )
)
(mcon 'leader
      '((superordinates (authority))
	(synonyms (arbiter ruler))
	(plural (leaders))
       )
)
(mcon 'leader-of
      '((synonyms (leader ruler-of head-of))
	(plural (leaders-of))
       )
)
(mcon 'leap
      '((synonyms (hop jump vault))
	(tenses (leaps lept leaping))
       )
)
(mcon 'leap-upon
      '((synonyms (leap leap-on jump-on jump-upon hop-on))
	(tenses (leaps-upon lept-upon leaping-upon))
       )
)
(mcon 'learn
      '((synonyms (memorize study discover realize see))
	(tenses (learns learned learing))
       )
)
(mcon 'learn-from
      '((synonyms (learn study-under))
	(tenses (learns-from learned-from learning-from))
       )
)
(mcon 'learn-how
      '((synonyms (learn discover-how))
	(tenses (learns-how learned-how learning-how))
       )
)
(mcon 'learn-to
      '((synonyms (learn))
	(tenses (learns-to learned-to learning-to))
       )
)
(mcon 'learning
      '((synonyms (enlightenment education instruction knowledge edification sophistication))
       )
)
(mcon 'leave
      '((synonyms (depart go quit retire retreat withdraw))
	(antonyms (remain stay))
	(tenses (leaves left leaving))
       )
)
(mcon 'leave-alone
      '((synonyms (leave ignore let-be))
	(antonyms (stay-with bother pester))
	(tenses (leaves-alone left-alone leaving-alone))
       )
)
(mcon 'leave-with
      '((synonyms (leave-behind leave-in-care-of leave))
	(tenses (leaves-with left-with leaving-with))
       )
)
(mcon 'leg
      '((superordinates (extension extremity limb))
	(synonyms (ankle calf drumstick limb shin thigh))
	(sub-parts (ankle calf foot forefoot forepaw hoof knee paw shin thigh))
	(part-of (bird body carcass figure fowl))
	(plural (legs))
       )
)
(mcon 'leg-of
      '((synonyms (leg calf-of limb-of))
	(sub-parts (ankle-of calf-of foot-of forefoot-of forepaw-of hoof-of knee-of paw-of shin-of thigh-of))
	(plural (legs-of))
       )
)
(mcon 'lend
      '((synonyms (afford furnish loan offer provide supply yield))
	(tenses (lends lent lending))
       )
)
(mcon 'letter
      '((synonyms (hand-letter print note message))
	(plural (letters))
       )
)
(mcon 'lie
      '((synonyms (fib untruth falsehood))
	(antonyms (truth verity))
	(plural (lies))
       )
)
(mcon 'lie-about
      '((synonyms (lie fib-about prevaricate-about))
	(tenses (lies-about lied-about lying-about))
       )
)
(mcon 'lie-to
      '((synonyms (lie fib-to prevaricate-to mislead))
	(tenses (lies-to lied-to lying-to))
       )
)
(mcon 'lieutenant
      '((superordinates (officer military-personnel))
	(synonyms (aide deputy))
	(plural (lieutenants))
       )
)
(mcon 'life
      '((synonyms (living vitality animation existence being))
	(antonyms (death dying decease demise))
	(plural (lives))
       )
)
(mcon 'life-of
      '((synonyms (life existence-of vitality-of))
	(antonyms (death-of dying-of demise-of))
	(plural (lives-of))
       )
)
(mcon 'lift-up
      '((synonyms (lift hoist hoist-up raise raise-up))
	(antonyms (lower drop))
	(tenses (lifts-up lifted-up lifting-up))
       )
)
(mcon 'lightbulb 
      '((synonyms (lamp light-source))
       )
)
(mcon 'like
      '((synonyms (admire esteem fancy favor love))
	(antonyms (detest dislike hate))
	(tenses (likes liked liking))
       )
)
(mcon 'lion
      '((superordinates (mammal feline))
	(synonyms (wild-cat))
	(plural (lions))
       )
)
(mcon 'lioness
      '((superordinates (mammal feline female))
	(synonyms (lion))
	(plural (lionesses))
       )
)
(mcon 'listen
      '((synonyms (hear attend heed))
	(antonyms (ignore))
	(tenses (listens listened listening))
       )
)
(mcon 'listen-to
      '((synonyms (listen heed pay-attention-to attend))
	(antonyms (ignore))
	(tenses (listens-to listened-to listening-to))
       )
)
(mcon 'live
      '((synonyms (be be-alive endure exist have-life living survive))
	(antonyms (die))
	(tenses (lives lived living))
       )
)
(mcon 'live-in
      '((superordinates (inhabit))
	(synonyms (live reside-in reside-at stay-at dwell-at occupy))
	(tenses (lives-in lived-in living-in))
       )
)
(mcon 'live-with
      '((synonyms (live cohabit reside-with stay-with))
	(tenses (lives-with lived-with living-with))
       )
)
(mcon 'load
      '((synonyms (burden cargo charge freight pack payload shipment weight))
	(plural (loads))
       )
)
(mcon 'load-of
      '((synonyms (load burden-of cargo-of charge-of freight-of pack-of payload-of))
	(plural (loads-of))
       )
)
; Actually, LOSE is mostly used in the sense of fail to keep or have taken
; away, but I figured I'd put this in anyway.
(mcon 'lose
      '((synonyms (misplace fail mislay let-slip fail-to-keep))
	(antonyms (find wind hold-on-to keep))
	(tenses (loses lost losing))
       )
)
(mcon 'lord
      '((superordinates (aristocrat blue-blood nobleman patrician))
	(subordinates (baron chief-of-state count crowned-head duke earl emperor grandee head-of-state king marquess marquis monarch prince sovereign viscount))
	(synonyms (governor master nobleman peer ruler))
	(plural (lords))
       )
)
(mcon 'love
      '((synonyms (adore respect honor dote like fancy favor idolize worship cherish desire))
	(antonyms (hate despise abhor detest dislike))
	(tenses (loves loved loving))
       )
)
(mcon 'lover
      '((superordinates (normal-person friend))
	(synonyms (sweetheart paramour suitor))
	(plural (lovers))
       )
)
(mcon 'low
      '((synonyms (depressed down downcast faint gentle inferior little low-set short small-amount-of small-degree))
	(antonyms (big high large tall))
       )
)
(mcon 'loyal
      '((synonyms (devoted faithful dedicated steadfast allegiant
		   dedicated true))
	(antonyms (disloyal mercurial unfaithful false))
       )
)
(mcon 'lucky
      '((synonyms (blessed fortunate))
	(antonyms (cursed unfortunate unlucky))
       )
)
(mcon 'lung
      '((part-of (respiratory-system mammal bird))
	(plural (lungs))
       )
)
(mcon 'lung-of
      '((plural (lungs-of))
       )
)
(mcon 'lure
      '((superordinates (trap))
	(synonyms (allure appeal attract bait bring-to captivate cause charm decoy draw enchant ensnare entice entrap fascinate induce inveigle invite lead prevail-upon seduce take tempt))
	(tenses (lures lured luring))
       )
)
(mcon 'luxurious
      '((synonyms (elaborate elegant lavish lush opulent plush ritzy))
	(antonyms (cheap poor))
       )
)
) ; end of semantics_l

(defun semantics_m ()

(mcon 'magician
      '((superordinates (entertainer))
	(synonyms (mage magus sorcerer necromancer wizard wonder-worker warlock conjurer diviner))
	(plural (magicians))
       )
)
(mcon 'maim
      '((synonyms (batter cripple damage disable incapacitate))
	(tenses (maims maimed maiming))
       )
)
(mcon 'make
      '((synonyms (build erect create invent produce form fabricate
		   forge construct initiate))
	(antonyms (destroy annihilate))
	(tenses (makes made making))
       )
)
(mcon 'make-angry
      '((synonyms (anger affront aggravate annoy antagonize bother displease disturb enrage exasperate gall infuriate irk irritate madden offend outrage peeve provoke upset vex))
	(tenses (makes-angry made-angry making-angry))
       )
)
(mcon 'make-false
      '((synonyms (prevent))
	(antonyms (make-true cause-to-be cause))
	(tenses (makes-false made-false making-false))
       )
)
(mcon 'make-tame
      '((synonyms (break-in civilize domesticate make-gentle make-meek refine soften subdue))
	(tenses (makes-tame made-tame making-tame))
       )
)
(mcon 'make-true
      '((synonyms (cause cause-to-be))
	(antonyms (make-false prevent))
	(tenses (makes-true made-true making-true))
       )
)
(mcon 'malignant
      '((synonyms (deadly virulent harmful malign unfriendly))
	(antonyms (benign harmless friendly))
       )
)
(mcon 'man
      '((superordinates (human person adult male))
	(synonyms (gentleman guy hombre))
	(plural (men))
       )
)
(mcon 'manger
      '((synonyms (booth stall crib))
	(plural (mangers))
       )
)
(mcon 'many
      '((synonyms (myriad numerous abundant plentiful multiple))
	(antonyms (few one scanty sparse))
       )
)
(mcon 'marble
      '((superordinates (stone))
	(subordinates (white-marble black-marble))
       )
)
(mcon 'marriage
      '((synonyms (matrimony wedlock))
       )
)
; NOTE: somehow we should be able to indicate that the adjective married and
;   the verb marry cause the same relationship between the people involved.
(mcon 'married
      '((synonyms (related wedded wed attached committed united))
	(antonyms (unwed uncommitted))
       )
)
(mcon 'marry
      '((synonyms (relate join unite couple mate wed))
	(tenses (marries married marrying))
       )
)
(mcon 'martin
      '((superordinates (swallow bird))
	(synonyms (house-martin))
	(plural (martins))
       )
)
(mcon 'mask
      '((superordinates (screen cover concealment protection))
	(synonyms (disguise))
	(plural (masks))
       )
)
(mcon 'master
      '((superordinates (authority educator instructor teacher))
	(subordinates (crowned-head emperor king monarch prince sovereign))
	(synonyms (boss commander employer lord maestro ruler superior virtuoso))
	(antonyms (employee inferior subject))
	(plural (masters))
       )
)
(mcon 'master-of
      '((superordinates (instructor-of teacher-of))
	(subordinates (emperor-of king-of monarch-of prince-of))
	(synonyms (master boss-of commander-of employer-of lord-of ruler-of superior-of))
	(antonyms (employee-of inferior-of subject-of))
	(plural (masters-of))
       )
)
(mcon 'masquerade
      '((synonyms (disguise parade))
	(tenses (masquerades masqueraded masquerading))
       )
)
(mcon 'meat
      '((superordinates (food nutriment))
	(subordinates (beef chop cutlet game mutton pork roast
		       sausage veal venison))
	(synonyms (flesh))
	(plural (meats))
       )
)
(mcon 'meddle-with
      '((synonyms (meddle fool-with tamper-with))
	(tenses (meddles-with meddled-with meddling-with))
       )
)
(mcon 'meekness
      '((synonyms (gentleness humbleness compliance submissiveness docility tameness humility modesty))
	(antonyms (defiance dominance pride))
       )
)
(mcon 'meet
      '((synonyms (assemble be-adequate be-adequate-to come-together come-upon come-up-to conform-to confront congregate contact convene converge encounter face fall-in-with find flock-together forgather form-a-junction fulfill gather get-together greet h

appen-upon join meet-with merge stumble-on tumble-upon turn-up unite welcome))
	(tenses (meets met meeting))
       )
)
(mcon 'meeting-of
      '((synonyms (meeting gathering-of convention-of assembly-of congregation-of))
	(plural (meetings-of))
       )
)
(mcon 'member
      '((synonyms (part affiliate insider belonger associate guildsman))
	(plural (members))
       )
)
(mcon 'member-of
      '((synonyms (member part-of affiliate-of associate-of))
	(plural (members-of))
       )
)
(mcon 'meow
      '((synonyms (mew miaow))
	(tenses (meows meowed meowing))
       )
)
(mcon 'merchant
      '((superordinates (professional-person))
	(subordinates (accountant actuary agent auditor baron big-businessman bookkeeper broker business-leader butcher clerk comptroller controller distributor examiner inspector intermediary jobber king magnate meatman meat-merchant middleman owner power propr

ietor top-executive tycoon wholesaler))
	(synonyms (businessman businessperson businesswoman entrepreneur industrialist))
	(plural (merchants))
       )
)
(mcon 'messenger
      '((superordinates (advocate interpreter representative spokesman))
	(synonyms (ambassador courier go-between herald intermediary))
	(plural (messengers))
       )
)
(mcon 'method
      '((synonyms (plan scheme idea program))
	(plural (methods))
       )
)
(mcon 'method-for
      '((synonyms (method plan-for scheme-for idea-for program-for))
	(plural (methods-for))
       )
)
(mcon 'mine
      '((superordinates (explosive-device))
	(subordinates (land_mine booby_trap floating_mine))
	(plural (mines))
       )
)
(mcon 'misanthrope
      '((superordinates (unpleasant-person cynic))
	(synonyms (misogynist man-hater hater misanthropist woman-hater))
	(plural (misanthropes))
       )
)
(mcon 'miserable
      '((synonyms (desolate joyless painful pitiful poor wretched))
	(antonyms (comfortable glad happy))
       )
)
(mcon 'misfortune
      '((synonyms (mishap misadventure mischance disaster calamity catastrophe cataclysm tragedy accident))
	(antonyms (luck fortune success))
	(plural (misfortunes)) ; Don't talk to me about these.
       )
)
(mcon 'mishandle
      '((synonyms (mismanage misconduct misgovern misrule misadminister))
	(tenses (mishandles mishandled mishandling))
       )
)
(mcon 'miss
      '((synonyms (omit pass pass-by leave-out fail-to-include))
	(antonyms (see observe))
	(tenses (misses missed missing))
       )
)
(mcon 'missile
      '((superordinates (weapon))
	(subordinates (arrow crossbow-bolt dart rocket))
	(synonyms (projectile))
	(plural (missiles))
       )
)
(mcon 'mistake
      '((synonyms (blunder failure))
	(plural (mistakes))
       )
)
(mcon 'mistletoe
      '((superordinates (bush plant))
	(sub-parts (flowers berries))
       )
)
(mcon 'mistreat
      '((synonyms (abuse harm hurt injure maltreat))
	(tenses (mistreats mistreated mistreating))
       )
)
(mcon 'mock
      '((synonyms (ape bamboozle banter befool burlesque challenge cheat con copy counterfeit cozen deceive defraud defy delude deride dummy dupe flaunt fleer flout fool gibe gird gull hoax hoodwink imitate impose-on insult jeer jeer-at jest kid make-fun 

mimic overreach parody phony play play-with provoke pull-one rally reproach ridicule scoff scoff-at scorn sham sneer swindle take-off taunt tease trick twit upbraid victimize))
	(antonyms (genuine))
	(tenses (mocks mocked mocking))
       )
)
(mcon 'mole
      '((superordinates (insect-eater insectivore mammal))
	(subordinates (shrew-mole star-nosed-mole))
	(plural (moles))
       )
)
(mcon 'money
      '((subordinates (metal-money coins bills))
	(synonyms (currency possession wealth))
       )
)
(mcon 'money-lender
      '((superordinates (professional-person))
	(synonyms (lender usurer banker money-broker loan-shark money-broker creditor))
       (plural (money-lenders))
      )
)
(mcon 'monkey
      '((superordinates (simian))
	(plural (monkeys))
       )
)
(mcon 'more
      '((synonyms (additionally additional greater extra))
	(antonyms (less fewer lesser))
       )
)
(mcon 'moor
      '((superordinates (race-of-person))
	(plural (moors))
       )
)
; don't know what else to say about the Moors.
(mcon 'most
      '((synonyms (greatest largest maximal))
	(antonyms (fewest least))
       )
)
(mcon 'mother
      '((superordinates (parent))
	(synonyms (mama mater mom mommy))
	(plural (mothers))
       )
)
(mcon 'mother-of
      '((superordinates (parent-of))
	(synonyms (mom-of mommy-of))
	(plural (mothers-of))
       )
)
(mcon 'mouse
      '((superordinates (gnawer rodent varmint mammal))
	(subordinates (deer-mouse field-mouse grasshopper-mouse white-footed-mouse wood-mouse))
	(plural (mice))
       )
)
(mcon 'mouse-hole
      '((superordinates (hole dwelling home house))
	(plural (mouse-holes))
       )
)
(mcon 'much
      '((antonyms (little))
       )
)
(mcon 'muddy
      '((synonyms (mire dirty bemire soil sully befoul))
	(tenses (muddies muddied muddying))
       )
)
(mcon 'mule
      '((superordinates (equid equine mammal))
	(plural (mules))
       )
)
(mcon 'must
      '((synonyms (have_to have_got_to ought should))
       )
)
(mcon 'mutually-exclusive
      '((antonyms (compatible))
       )
)
) ; end of semantics_m

