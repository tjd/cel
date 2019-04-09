
(defun semantics_s ()

; OOPS -- sacrifice is both one place and two -- noun and verb -- this is both,
; for now.
(mcon 'sacrifice
      '((synonyms (offer offering))
	(plural (sacrifices))
	(tenses (sacrifices sacrificed sacrificing))
       )
)
(mcon 'safe
      '((synonyms (exempt free  secure sheltered sound spared stable))
	(antonyms (harmed insecure susceptible unprotected unsafe vulnerable))
       )
)
(mcon 'sail
      '((synonyms (glide move-through-air move-over-water navigate sheet soar voyage wing))
	(tenses (sails sailed sailing))
       )
)
(mcon 'sail-on
      '((synonyms (sail float-on ride-on glide-through navigate-on))
	(tenses (sails-on sailed-on sailing-on))
       )
)
(mcon 'same
      '((synonyms (equivalent identical indistinguishable alike similar equal interchangeable))
	(antonyms (different distinguishable distinct unique unequal))
       )
)
(mcon 'same-as
      '((synonyms (same equivalent-to identical-to similar-to equal-to interchangeable-with))
        (antonyms (different-from distinct-from unequal-to))
       )
)
(mcon 'savage
      '((synonyms (barbarian barbaric brutal cruel ferocious fierce harsh pitiless sadistic uncivilized vicious violent wild))
	(antonyms (civil civilized humane kind merciful tame))
       )
)
(mcon 'save
      '((synonyms (spare protect rescue reclaim))
	(tenses (saves saved saving))
       )
)
(mcon 'save-from
      '((synonyms (save protect-from rescue-from))
	(tenses (saves-from saved-from saving-from))
       )
)
(mcon 'savior
      '((synonyms (keeper rescuer redeemer emancipator))
        (plural (saviors))
       )
)
(mcon 'savior-of
      '((synonyms (keeper-of rescuer-of redeemer-of emancipator-of))
        (plural (saviors-of))
       )
)
(mcon 'say
      '((synonyms (assert claim declare speak state talk))
	(tenses (says said saying))
       )
)
(mcon 'say-to
      '((synonyms (say declare-to state-to))
	(tenses (says-to said-to saying-to))
       )
)
(mcon 'scare
      '((synonyms (affright alarm frighten panic rouse startle terrify))
	(antonyms (calm pacify))
	(tenses (scares scared scaring))
       )
)
(mcon 'scold
      '((synonyms (abuse berate reproach nag rebuke chide))
	(tenses (scolds scolded scolding))
       )
)
(mcon 'scorn
      '((synonyms (mock insult deride patronize disdain spurn))
	(tenses (scorns scorned scorning))
       )
)
(mcon 'sea
      '((synonyms (ocean))
	(antonyms (land earth air sky))
	(part-of (earth world))
	(sub-parts (water waves))
	(plural (seas))
       )
)
(mcon 'sealed
      '((synonyms (caulked plugged closed airtight hermetic tight 
		   watertight impermeable impervious  fastened fixed
		   locked secured shut unopened))
	(antonyms (unsealed leaky))
       )
)
(mcon 'search
      '((synonyms (beat check-into comb dig-into examine explore ferret-out forage hunt inquire-into investigate look-for patrol penetrate-into poke-around poke-into probe prospect range range-over ransack reconnoiter reconnoitre rummage scan scour scout 

scrounge scrutinize search-for search-into search-out search-through seek study survey))
	(tenses (searches searched searching))
       )
)
(mcon 'search-for
      '((synonyms (search hunt look-for search-out seek seek-out))
	(tenses (searches-for searched-for searching-for))
       )
)

(mcon 'seashore
      '((synonyms (shore coast strand beach seacoast seaside seaboard))
	(plural (seashores))
       )
)
(mcon 'secret
      '((synonyms (unknown hidden concealed surreptitious confidential private concealed disguised))
	(antonyms (known shared public open))
       )
)
(mcon 'security
      '((synonyms (comfort contentment ease))
	(antonyms (discomfort fear))
       )
)
(mcon 'sedate
      '((synonyms (calm composed dignified dispassionate even-tempered grave level-headed poised quiet sober solemn tranquil))
	(antonyms (agitated emotional gay indecorous passionate))
       )
)
(mcon 'seduce
      '((synonyms (allure attract cajole coax con copulate-with deceive decoy draw entice inveigle lay lead-astray lure make persuade tempt))
	(tenses (seduces seduced seducing))
       )
)
(mcon 'see
      '((synonyms (apprehend behold notice observe perceive sense spy view))
	(tenses (sees saw seeing))
       )
)
(mcon 'see-in
      '((synonyms (see behold-in notice-in observe-in spy-in view-in))
	(tenses (sees-in saw-in seeing-in))
       )
)
(mcon 'see-that
      '((synonyms (see observe-that perceive-that notice-that))
	(tenses (sees-that saw-that seeing-that))
       )
)
(mcon 'seek
      '((synonyms (look-for pursue search-for search-out seek-after seek-for))
	(tenses (seeks sought seeking))
       )
)
(mcon 'seize
      '((synonyms (acquire apprehend appropriate arrest capture catch clasp clutch commandeer confiscate ensnare gain get grab grasp grip hug nab snare snatch steal take))
	(tenses (seizes seized seizing))
       )
)
(mcon 'self-interested
      '((synonyms (egotistical egocentric narcissistic selfish self-centered))
	(antonyms (unselfish self-sacrificing selfless altruistic humble modest))
       )
)
(mcon 'sell
      '((synonyms (deal peddle vend market))
	(tenses (sells sold selling))
       )
)
(mcon 'sell-to
      '((synonyms (sell))
	(tenses (sells-to sold-to selling-to))
       )
)
(mcon 'senator
      '((superordinates (official public-servant politician aristocrat lawmaker))
	(synonyms (assemblyman councilman representative))
	(plural (senators))
       )
)
(mcon 'send
      '((synonyms (appoint assign commend commission delegate direct dispatch))
	(tenses (sends sent sending))
       )
)
(mcon 'send-to
      '((synonyms (send deliver-to direct-to dispatch-to mail-to ship-to transmit-to transport-to))
	(tenses (sends sent sending))
       )
)
(mcon 'senseless
      '((synonyms (foolhardy rash unreasonable foolish illogical))
	(antonyms (logical wise intelligent thoughtful))
      )
)
(mcon 'sensible
      '((synonyms (considered discreet judicious level-headed logical prudent rational reasonable sane sound well-advised wise))
	(antonyms (ill-advised illogical imprudent inadvisable irrational unreasonable unsound unwise))
       )
)
(mcon 'senile
      '((synonyms (aging anile doddering doddery doting decrepit feeble-minded senescent))
	(antonyms (young sensible))
       )
)
(mcon 'separate
      '((synonyms (break break-up cleave cut cut-up detach disconnect disjoin divide divorce free part section sever split split-up split-apart))
	(antonyms (attach connect join unite bring-together))
	(tenses (separates separated separating))
       )
)
(mcon 'separated
      '((synonyms (apart disengaged disjunct dispatched distant divided isolated parted partitioned partitioned-off remote removed sectioned separate set-apart severed sundered unattached withdrawn))
	(antonyms (attached collected connected integrated joint united))
       )
)
(mcon 'serious
      '((synonyms (deep dour earnest grave grievous intense profound severe sincere sober solemn weighty))
	(antonyms (frivolous gay humorous indulgent light-hearted minor playful superficial unimportant))
       )
)
(mcon 'serpent
      '((superordinates (reptile))
	(synonyms (snake viper))
	(plural (serpents))
       )
)
(mcon 'servant
      '((superordinates (employee working-person wage-earner))
	(subordinates (domestic maid manservant menial))
	(synonyms (retainer))
	(plural (servants))
       )
)
(mcon 'serve
      '((synonyms (aid answer attend attend-to avail benefit be-adequate be-dutiful-to be-of-use be-servant-to care-for come-up-to discharge-a-duty do fulfill function further have-a-function help look-after minister-to obey tend wait-on wait-tables work-

for))
	(tenses (serves served serving))
       )
)
(mcon 'serve-on
      '((synonyms (serve present-on dispense-on))
	(tenses (serves-on served-on serving-on))
       )
)
(mcon 'serve-to
      '((synonyms (serve give-to offer-to present-to))
	(tenses (serves-to served-to serving-to))
       )
)
(mcon 'service
      '((synonyms (task job deed))
	(plural (services))
       )
)
(mcon 'sex
      '((synonyms (copulation sex-act coitus intercourse sexual-intercourse sexual-union sexual-relations lovemaking))
       )
)
(mcon 'shadow
      '((synonyms (cloud darkness dim shade silhouette))
	(plural (shadows))
       )
)
(mcon 'share
      '((synonyms (apportion distribute divide divide-up parcel parcel-out share-in split split-up))
	(tenses (shares shared sharing))
       )
)
(mcon 'share-with
      '((synonyms (share give-to split-with))
	(tenses (shares-with shared-with sharing-with))
       )
)
(mcon 'sharp
      '((synonyms (acute barbed pointed))
        (antonyms (dull toothless))
       )
)
(mcon 'sharpen
      '((synonyms (grind hone make-sharper polish refine whet))
	(tenses (sharpens sharpened sharpening))
       )
)
(mcon 'sharper
      '((superordinates (more sharp))
	(antonyms (duller))
       )
)
(mcon 'shear
      '((synonyms (clip crop fleece trim))
	(tenses (shears sheared shearing))
       )
)
(mcon 'sheep
      '((superordinates (bovid mammal))
	(subordinates (ewe lamb ram))
	(plural (sheep))
       )
)
(mcon 'sheepskin
      '((superordinates (leather hide))
	(part-of (sheep clothing))
	(plural (sheepskins))
       )
)
(mcon 'shepherd
      '((superordinates (herdsman))
	(synonyms (shepherdess sheepherder))
       )
)
(mcon 'shepherdess
      '((superordinates (herdsman))
	(synonyms (shepherd sheepherder))
	(plural (shepherdesses))
       )
)
; SHIP - pared down.
(mcon 'ship
      '((superordinates (vessel))
	(subordinates (battleship cargo-ship cargo-vessel cruiser frigate liner ocean-liner passenger-ship sailboat sub submarine warship))
	(synonyms (boat))
	(sub-parts (aft anchor brig capstan cargo-area cargo-deck deck fin fins forecastle galley hold log mainsail mainsheet mooring poop propeller quarter rear sail stern winch windlass))
	(plural (ships))
       )
)
(mcon 'simple
      '((synonyms (crude easy elemental elementary fundamental noncompound obvious plain primary primitive rudimentary straightforward unadorned uncomplicated))
	(antonyms (adorned cluttered complex difficult fancy obscure sophisticated unclear))
       )
)
(mcon 'shoot
      '((synonyms (strike wound kill))
	(tenses (shoots shot shooting))
       )
)
(mcon 'shoot-at
      '((superordinates (attack))
	(synonyms (shoot hunt fire-upon fire-at))
	(tenses (shoots-at shot-at shooting-at))
       )
)
(mcon 'show
      '((synonyms (convey display exhibit flaunt proclaim signify))
	(antonyms (conceal hide))
	(tenses (shows showed showing))
       )
)
(mcon 'show-that
      '((synonyms (show convey-that proclaim-that signify-that))
	(antonyms (conceal-that hide-that))
	(tenses (shows-that showed-that showing-that))
       )
)
(mcon 'shrew
      '((superordinates (unpleasant-person))
	(synonyms (bitch vixen scold nag))
	(plural (shrews))
       )
)
(mcon 'sibling
      '((superordinates (person relation relative))
	(synonyms (brother sister kin kinsman blood-relative))
	(plural (siblings))
       )
)
(mcon 'sick
      '((synonyms (ailing diseased ill sickly unwell))
	(antonyms (healthy well sound))
       )
)
(mcon 'sing
      '((synonyms (chant intone warble))
	(tenses (sings sang singing))
       )
)
(mcon 'slab
      '((synonyms (wood stone board))
	(plural (slabs))
       )
)
(mcon 'slave
      '((superordinates (unfortunate-person worker))
	(synonyms (subject vassal captive chattel servant serf peon))
	(plural (slaves))
       )
)
(mcon 'slave-of
      '((synonyms (slave subject-of servant-of serf-to captive-of))
	(plural (slaves-of))
       )
)
(mcon 'sleep
      '((synonyms (doze hibernate lie lodge nap repose rest slumber snooze))
	(tenses (sleeps slept sleeping))
       )
)
(mcon 'sleep-in
      '((synonyms (sleep doze-in hibernate-in lie-in lodge-in nap-in repose-in rest-in slumber-in snooze-in))
	(tenses (sleeps-in slept-in sleeping-in))
       )
)
(mcon 'sleep-under
      '((synonyms (sleep sleep-below doze-under lie-under nap-under rest-under slumber-under snooze-under))
	(tenses (sleeps-under slept-under sleeping-under))
       )
)
(mcon 'slowly
      '((synonyms (by-degrees gradually step-by-step bit-by-bit slow sluggishly languidly leisurely unhurriedly lingeringly haltingly falteringly in-slow-motion))
       )
)
(mcon 'small
      '((synonyms (tiny minure miniscule petit miniature diminuative little))
	(antonyms (big large huge vast immense enormous))
       )
)
(mcon 'smartest
      '((superordinates (most smart))
	(synonyms (brightest quickest shrewdest craftiest wisest))
       )
)
(mcon 'smother
      '((synonyms (asphyxiate blanket cover crush quash quell quench silence stifle subdue suffocate suppress))
	(tenses (smothers smothered smothering))
       )
)
(mcon 'snake
      '((superordinates (reptile))
	(subordinates (blacksnake boa cobra coral-snake king-cobra pit-viper rattler rattlesnake viper))
	(plural (snakes))
       )
)
(mcon 'snare
      '((superordinates (trap))
	(synonyms (gin noose))
	(plural (snares))
       )
)
(mcon 'snatch
      '((synonyms (catch clasp clutch get grab grasp grip kidnap seize steal take))
        (tenses (snatches snatched snatching))
       )
)
(mcon 'snatch-from
      '((synonyms (snatch get-from grab-from kidnap-from seize-from steal-from take-from))
        (tenses (snatches-from snatched-from snatching-from))
       )
)
(mcon 'so-that
      '((synonyms (to in-order-to in-order-that so-as-to))
       )
)
(mcon 'soldier
      '((superordinates (military-personnel))
	(subordinates (buck-private common-soldier enlisted-man private officer))
	(synonyms (warrior))
	(plural (soldiers))
       )
)
(mcon 'solitude
      '((synonyms (aloneness loneliness privacy seclusion isolation))
       )
)
(mcon 'solve
      '((synonyms (answer resolve unfold unravel unriddle work))
	(tenses (solves solved solving))
       )
)
(mcon 'some
      '((synonyms (a-few little multiple numerous part-of several))
        (antonyms (few many))
       )
)
(mcon 'some-of
      '((synonyms (some a-few-of little-of part-of several-of))
	(antonyms (few-of many-of))
       )
)
(mcon 'son
      '((superordinates (boy man person child kin relative))
	(synonyms (descendant offspring scion sonny))
	(plural (sons))
       )
)
(mcon 'sonnet
      '((synonyms (poem verse rhyme))
        (plural (sonnets))
       )
)
(mcon 'sorrowful
      '((synonyms (unhappy mournful doleful sad desolate joyless))
	(antonyms (glad happy joyful pleased))
       )
)
(mcon 'sound-source
      '((superordinates (source device))
	(plural (sound-sources))
       )
)
(mcon 'soup
      '((superordinates (food))
	(subordinates (borscht bouillon broth chicken-soup chowder gumbo minestrone oxtail-soup stock turtle-soup vegetable-soup))
	(synonyms (potage))
	(plural (soups))
       )
)
(mcon 'sour
      '((synonyms (acid rancid tangy tart))
	(antonyms (sweet))
       )
)
(mcon 'source
      '((synonyms (producer originator origin emitter))
	(plural (sources))
       )
)
(mcon 'spare
      '((synonyms (save release show-mercy forgive protect pardon))
	(tenses (spares spared sparing))
       )
)
(mcon 'spirit
      '((superordinates (supernatural supernatural-being))
	(plural (spirits))
       )
)
(mcon 'spoil
      '((synonyms (blemish blight confound corrupt damage defile deform degrade injure mangle mar mess-up mutilate ravage ruin taint))
	(tenses (spoils spoiled spoiling))
       )
)
(mcon 'sportsman
      '((superordinates (human person))
	(synonyms (athlete jock player hunter gamesman))
	(plural (sportsmen))
       )
)
(mcon 'spring
      '((superordinates (water-source))
	(subordinates (hot-spring mineral-spring))
	(sub-parts (water))
	(plural (springs))
       )
)
(mcon 'stab
      '((synonyms (cause-to-penetrate cause-to-pierce cut dash drive gore hit hurl impel jab knife lacerate penetrate pierce poke puncture spear stick thrust wound))
	(tenses (stabs stabed stabbing))
       )
)
(mcon 'stag
      '((superordinates (male mammal))
	(synonyms (hart buck))
	(plural (stags))
       )
)
(mcon 'statement
      '((synonyms (declaration pronouncement sentence))
	(plural (statements))
       )
)
(mcon 'statue-of
      '((superordinates (sculpture))
	(synonyms (statue))
        (plural (statues))
       )
)
(mcon 'stay-in
      '((synonyms (remain tarry delay pause await wait abide linger))
        (tenses (stays-in stayed-in staying-in))
       )
)
(mcon 'steal
      '((synonyms (acquire appropriate filch get nab obtain pilfer purloin seize snatch swipe take))
	(antonyms (relinquish resign return))
	(tenses (steals stole stealing))
       )
)
(mcon 'steal-from
      '((synonyms (steal snatch-from swipe-from take-from))
	(antonyms (return-to give-back-to))
	(tenses (steals-from stole-from stealing-from))
       )
)
(mcon 'stepson
      '((superordinates (boy man person child relative))
	(synonyms (stepchild))
	(plural (stepsons))
       )
)
(mcon 'steward
      '((superordinates (servant))
	(synonyms (majordomo butler housekeeper))
	(plural (stewards))
       )
)
(mcon 'sting
      '((superordinates (attack))
	(synonyms (bite poke prick))
	(tenses (stings stung stinging))
       )
)
(mcon 'stork
      '((superordinates (bird))
	(plural (storks))
       )
)
(mcon 'storm
      '((synonyms (tempest squall tornado cyclone hurricane typhoon stormy-weather rough-weather foul-weather))
	(subordinates (rainstorm windstrom hailstorm thunderstorm))
	(plural (storms))
       )
)
(mcon 'stream
      '((superordinates (waterway watercourse))
	(synonyms (river brook creek))
	(plural (streams))
       )
)
(mcon 'strife
      '((synonyms (discord conflict contention dissent))
	(antonyms (agreement peace))
       )
)
(mcon 'strong
      '((synonyms (tough mighty powerful robust sturdy durable lasting solid stable ironclad unshakable healthy intense secure))
	(antonyms (weak delicate ineffective unhealthy insecure impotent unstable))
       )
)
(mcon 'stronger
      '((superordinates (more strong))
	(synonyms (tougher mightier sturdier healthier))
	(antonyms (weaker))
       )
)
(mcon 'stronger-than
      '((superordinates (more-than strong))
	(synonyms (tougher-that mightier-than sturdier-than healthier-than))
	(antonyms (weaker-than))
       )
)
(mcon 'student
      '((superordinates (normal-person young-person))
	(synonyms (apprentice pupil scholar))
	(plural (students))
       )
)
(mcon 'study
      '((synonyms (analyze check-into cogitate con consider contemplate deliberate dig-into examine excogitate explore go-over inquire-into inspect investigate learn meditate-upon memorize observe peruse poke-into ponder probe reflect-on reflect-upon rese

arch revolve scan screen scrutinize search search-exhaustively search-into search-through sift survey think think-about view weigh))
	(tenses (studies studied studying))
       )
)
(mcon 'studying
      '((synonyms (learning investigating pondering gaining-knowledge))
       )
)
(mcon 'stupid
      '((synonyms (dull feebleminded retarded unintelligent witless))
	(antonyms (clever intelligent sharp))
       )
)
(mcon 'substitute
      '((synonyms (alternate anaphoric change deputy displace exchange proxy replace second-string shift stand-in standby supply switch transfer utility vicarious vice))
	(tenses (substitutes substituted substituting))
       )
)
(mcon 'succeed
      '((synonyms (flourish thrive triumph))
	(antonyms (fail lose))
	(tenses (succeeds succeeded succeeding))
       )
)
(mcon 'suffer
      '((synonyms (ache feel-pain))
	(tenses (suffers suffered suffering))
       )
)
(mcon 'suggest
      '((synonyms (advise recommend propose present prompt urge))
	(tenses (suggests suggested suggesting))
       )
)
(mcon 'suggest-that
      '((synonyms (suggest advise-that recommend-that propose-that urge-that))
	(tenses (suggests-that suggested-that suggesting-that))
       )
)
(mcon 'suggest-to
      '((synonyms (suggest recommend-to propose-to present-to))
        (tenses (suggests-to suggested-to suggesting-to))
       )
)
(mcon 'sun
      '((superordinates (star heavenly-body))
        (sub-parts (corona sunspot solar-flare))
	(plural (suns))
       )
)
(mcon 'superior-to
      '((synonyms (greater-than superior better-than finer-than))
	(antonyms (worse-than inferior-to inferior coarser-than lesser))
       )
)
(mcon 'support
      '((synonyms (abet accommodate aid assist back back-financially back-up bolster collaborate cooperate corroborate favor finance help nourish patronize provide-for reassure reinforce sponsor succor supply supply-sustenance sustain))
	(tenses (supports supported supporting))
       )
)
(mcon 'surprise
      '((synonyms (astonish amaze astound startle confound boggle dazzle))
	(tenses (surprises surprised surprising))
       )
)
(mcon 'surprised-that
      '((synonyms (surprised astonished-that amazed-that astounded-that startled-that))
       )
)
(mcon 'surrender
      '((synonyms (abandon-oneself abdicate abnegate allow bow capitulate cede commit concede consign convey decline defer deliver devote-oneself forgo forsake give give-in give-over give-up grant hand-over part-with permit quit release relent relinquish 

render renounce resign submit succumb vacate waive yield))
	(tenses (surrenders surrendered surrendering))
       )
)
(mcon 'surround
      '((synonyms (border edge surround skirt circle enclose surround encircle ring girdle gird encompass))
       )
)
(mcon 'survive
      '((synonyms (last live endure pull-through))
       )
)
(mcon 'suspicious
      '((synonyms (distrustful dubious questionable shady suspect untrusting wary))
        (antonyms (careless incautious trustful))
       )
)
(mcon 'suspicious-of
      '((synonyms (suspicious distrustful-of dubious-of untrusting-of wary-of))
	(antonyms (careless-with trustful-of))
       )
)
(mcon 'swallow
      '((synonyms (eat ingest drink swig imbibe gulp))
	(tenses (swallows swallowed swallowing))
       )
)
(mcon 'swan
      '((superordinates (bird))
	(subordinates (trumpeter trumpeter-swan))
	(plural (swans))
       )
)
(mcon 'swift
      '((synonyms (fleet prompt quick rapid speedy))
        (antonyms (gradual slow))
       )
)
(mcon 'swifter
      '((superordinates (more swift))
	(synonyms (quicker rapider speedier))
	(antonyms (slower))
       )
)
) ; end of semantics_s

(defun semantics_t ()

(mcon 'tail
      '((superordinates (body-part))
	(part-of (rump animal))
	(plural (tails))
       )
)
(mcon 'take
      '((synonyms (acquire appropriate filch get nab obtain pilfer remove seize snatch steal))
	(antonyms (give relinquish resign return))
	(tenses (takes took taking))
       )
)
(mcon 'take-from
      '((synonyms (take get-from nab-from obtain-from remove-from snatch-from steal-from))
	(antonyms (give-to relinquish-to return-to))
	(tenses (takes-from took-from taking-from))
       )
)
(mcon 'talk
      '((synonyms (chat chatter converse discourse prate prattle speak))
	(tenses (talks talked talking))
       )
)
(mcon 'tame
      '((synonyms (amenable civilized docile gentle humble meek obedient refined submissive))
	(antonyms (defiant dominant unmanageable wild))
       )
)
(mcon 'task
      '((synonyms (undertaking enterprise operation venture work project affair business job chore assignment charge mission duty labor toil))
	(plural (tasks))
       )
)
(mcon 'teach
      '((synonyms (coach cultivate discipline educate emplant exhort expound-scripture impart-knowledge implant inculcate infix inform inseminate instill instruct preach profess prophesy propound school show train tutor))
	(tenses (teaches taught teaching))
       )
)
(mcon 'teach-how
      '((synonyms (teach inform-how preach-how show-how))
	(tenses (teaches-how taught-how teaching-how))
       )
)
(mcon 'tear-down
      '((synonyms (demolish destroy pull-down raze wreck))
	(tenses (tears-down tore-down tearing-down))
       )
)
(mcon 'tell
      '((synonyms (confess declare disclose divulge express impart relate say state utter voice))
	(tenses (tells told telling))
       )
)
(mcon 'tell-to-do
      '((synonyms (tell advise-to bid-to charge-to command-to enjoin-to instruct-to order-to))
	(tenses (tells-to told-to telling-to))
       )
)
(mcon 'temple
      '((synonyms (house-of-god house-of-prayer house-of-worship place-of-worship))
	(plural (temples))
       )
)
(mcon 'tempt
      '((synonyms (lure entice attract draw))
	(tenses (tempts tempted tempting))
       )
)
(mcon 'test
      '((synonyms (assay evaluation examination experiment))
	(plural (tests))
       )
)
(mcon 'thane
      '((superordinates (nobleman noble))
	(synonyms (lord baron))
	(plural (thanes))
       )
)
(mcon 'thief
      '((superordinates (criminal felon lawbreaker outlaw))
        (subordinates (highwayman shoplifter))
	(synonyms (crook robber))
        (plural (thieves))
       )
)
(mcon 'thing
      '((subordinates (living-thing non-living-thing object organism))
	(synonyms (entity))
	(plural (things))
       )
)
(mcon 'think
      '((synonyms (conceive imagine ponder reason reflect))
	(tenses (thinks thought thinking))
       )
)
; UGH!  There's nothing reasonable to put in for THIRSTY!
(mcon 'thirsty
      '((antonyms (quenched))
       )
)
(mcon 'though
      '((synonyms (still although even-though))
       )
)
(mcon 'threaten
      '((synonyms (alarm menace confront warn frighten))
	(tenses (threatens threatened threatening))
       )
)
(mcon 'throne
      '((superordinates (chair))
	(synonyms (seat-of-power rulership))
	(plural (thrones))
       )
)
(mcon 'tick
      '((superordinates (arachnid insect))
	(plural (ticks))
       )
)
(mcon 'tie-to
      '((synonyms (tie attach-to connect-to link-to))
	(tenses (ties-to tied-to tying-to))
       )
)
(mcon 'tie-together
      '((synonyms (tie attach harness link yoke))
	(tenses (ties-together tied-together tying-together))
       )
)
; This is used in two slightly different senses -- both one place abstract
; nouns.  Hmm...
(mcon 'time
      '((synonyms (period point-in-time))
	(plural (times))
       )
)
(mcon 'tire
      '((synonyms (bore distress do do-in ennui exhaust fag fatigue harras irk jade overwork satiate tucker wear weary wear-out wind))
	(tenses (tires tired tiring))
       )
)
(mcon 'tissue
      '((superordinates (body_substance))
	(subordinates (adipose_tissue fat marrow connective_tissue 
		       lymphatic_tissue cartilage erectile_tissue 
		       muscle fibrous_tissue nervous_tissue gingiva 
		       gum dentin membrane tissue_layer))
       )
)
(mcon 'together
      '((synonyms (collectively concurrently cooperatively jointly
		   simultaneously unanimously))
	(antonyms (apart separately))
       )
)
(mcon 'toil
      '((synonyms (drudge labor struggle work))
	(plural (toils))
       )
)
(mcon 'tomb
      '((superordinates (burial-chamber))
	(subordinates (sepulcher sepulchre))
	(synonyms (grave burial))
	(sub-parts (gravestone headstone tombstone))
	(plural (tombs))
       )
)
(mcon 'tooth
      '((subordinates (canine dogtooth eyetooth incisor))
	(synonyms (fang molar))
	(part-of (mouth snout muzzle animal))
	(plural (teeth))
      )
)
(mcon 'tooth-of
      '((synonyms (tooth fang-of))
	(plural (teeth-of))
       )
)
(mcon 'torment
      '((synonyms (harass annoy plague torture afflict pain))
	(tenses (torments tormented tormenting))
       )
)
(mcon 'tortoise
      '((superordinates (reptile))
	(subordinates (box-tortoise box-turtle sea-turtle snapping-turtle terrapin))
	(synonyms (turtle))
	(plural (tortoises))
       )
)
(mcon 'tournament
      '((synonyms (contest match game games tourney))
	(plural (tournaments))
       )
)
(mcon 'town
      '((synonyms (village community neighborhood city metropolis))
	(plural (towns))
       )
)
(mcon 'track
      '((synonyms (footprint footstep pawmark pawprint))
	(plural (tracks))
       )
)
(mcon 'trait
      '((synonyms (character-trait personality-trait))
	(plural (traits))
       )
)
(mcon 'traitor
      '((superordinates (bad-person))
	(synonyms (apostate turncoat defector deserter))
	(plural (traitors))
       )
)
(mcon 'traitor-to
      '((synonyms (traitor))
	(plural (traitors-to))
       )
)
(mcon 'trap
      '((subordinates (snare noose))
	(sub-parts (bait decoy lure))
	(plural (traps))
       )
)
(mcon 'trap-for
      '((subordinates (snare-for noose-for))
	(synonyms (trap))
	(sub-parts (bait decoy lure))
	(plural (traps-for))
       )
)
(mcon 'travel
      '((synonyms (be-in-motion cover cruise explore go jaunt journey journey-through move move-along pass pass-over peregrinate proceed roll run tour track traverse trip voyage))
	(tenses (travels traveled travelling))
       )
)
(mcon 'treacherous
      '((synonyms (cunning deceitful guileful insidious malicious perfidious sly underhand unfaithful venomous))
	(antonyms (faithful reliable trustworthy))
       )
)
(mcon 'tread
      '((synonyms (leap lumber pace stamp step tramp walk))
        (tenses (treads trod treaded treading))
       )
)
(mcon 'tread-on
      '((synonyms (tread stamp-on step-on tramp-on trample-on walk-on))
        (tenses (treads-on trod-on treaded-on treading-on))
       )
)
(mcon 'treasure
      '((superordinates (valuables money))
	(synonyms (fortune wealth riches possessions gains))
	(plural (treasures))
       )
)
(mcon 'treat
      '((synonyms (deal-with attend-to cope-with tend attend care-for minister-to heal))
	(tenses (treats treated treating))
       )
)
(mcon 'treatment
      '((superordinates (medication medicine))
	(synonyms (corrective restorative))
	(plural (treatments))
       )
)
(mcon 'tree
      '((superordinates (vegetation plant))
	(synonyms (timber))
	(subordinates (shade-tree fruit-tree timber-tree softwood-tree hardwood-tree sapling seedling pollard conifer evergreen deciduous-tree))
        (plural (trees))
       )
)
(mcon 'trick
      '((synonyms (defraud deceive dupe con hoodwink bamboozle))
	(tenses (tricks tricked tricking))
       )
)
(mcon 'triumvir
      '((synonyms (ruler))
	(part-of (triumvirate))
	(plural (triumvirs))
       )
)
(mcon 'trouble
      '((synonyms (affliction bother burden concern discomfort distress disturbance inconvenience pain perturbance worry))
	(plural (troubles))
       )
)
(mcon 'truce
      '((superordinates (agreement))
	(synonyms (amistice peace treaty cease-fire suspension-of-hostilities))
	(plural (truces))
       )
)
(mcon 'truth
      '((synonyms (verity fact reality veracity truism))
	(antonyms (falsehood untruth lie falsity fallacy))
	(plural (truths))
       )
)
(mcon 'trust
      '((synonyms (believe count-on entrust rely-on))
	(tenses (trusts trusted trusting))
       )
)
(mcon 'tumor
      '((superordinates (disease-symptom))
	(synonyms (growth cancer cancerous-growth))
       )
)
(mcon 'tusk
      '((superordinates (bone tooth))
        (synonyms (ivory))
        (part-of (jaw mouth))
        (plural (tusks))
       )
)
(mcon 'tusk-of
      '((superordinates (bone-of tooth-of))
	(part-of (jaw-of mouth-of))
        (plural (tusks-of))
       )
)
(mcon 'tutor
      '((synonyms (coach educator instructor pedagogue teacher private-instructor))
	(plural (tutors))
       )
)
(mcon 'twice
      '((synonyms (doubly two-times))
	(antonyms (once often more-than-twice))
       )
)
(mcon 'twin
      '((superordinates (human-being person sibling relative kin))
	(plural (twins))
       )
)
(mcon 'tyrannical
      '((synonyms (despotic dictatorial ruthless tyrannous))
	(antonyms (democratic fair gentle just kind))
       )
)
) ; end of semantics_t

(defun semantics_uv ()

(mcon 'ultrasound
      '((superordinates (vibrations sound))
       )
)
(mcon 'unable
      '((synonyms (helpless incapable incompetent powerless unqualified))
	(antonyms (able capable fit powerful puissant))
       )
)
(mcon 'unaware
      '((synonyms (unsuspecting uninformed unadvised incognizant unknowing))
       )
)
(mcon 'undesirable
      '((synonyms (unattractive unfavorable objectionable unpleasant unacceptable))
	(antonyms (desirable favorable))
       )
)
(mcon 'unfair
      '((synonyms (differential discriminative discriminatory foul inequitable partial prejudiced treacherous uneven unjust unreasonable))
	(antonyms (equal fair honest honorable impartial just progressive right))
       )
)
(mcon 'unfaithful
      '((synonyms (disloyal faithless false fickle inconstant light perfidious treacherous untrue versatile))
	(antonyms (accurate constant dependable enduring faithful loyal reliable true))
       )
)
(mcon 'unit
      '((subordinates (organization outfit division company platoon battery squad))
	(synonyms (piece one-of individual))
	(plural (units))
       )
)
(mcon 'unite
      '((synonyms (adhere affix ally attach blend cement combine compound concur connect consolidate cooperate couple fasten fuse hitch integrate join link merge tether tie unify))
        (tenses (unites united uniting))
       )
)
(mcon 'unite-with
      '((synonyms (unite affix-to ally-with attach-to blend-with combine-with concur-with consolidate-with cooperate-with integrate-with join-with merge-with unify-with))
        (tenses (unites-with united-with uniting-with))
       )
)
(mcon 'unjustified
      '((synonyms (indefensible needless uncalled-for unmerited unwarranted))
	(antonyms (justified necessary))
       )
)
(mcon 'unkind
      '((synonyms (hardhearted intolerant severe uncompassionate unfeeling unmerciful unsympathetic))
        (antonyms (charitable considerate kind merciful warmhearted))
       )
)
(mcon 'unkind-to
      '((synonyms (unkind intolerant-of severe-to unmerciful-to unsympathetic-to))
	(antonyms (charitable-to considerate-of kind-to merciful-to))
       )
)
(mcon 'unless
      '((synonyms (excluding excepting save))
	(antonyms (if when))
       )
)
(mcon 'unlucky
      '((synonyms (infelicitous unfortunate))
	(antonyms (blessed fortunate lucky successful))
       )
)
(mcon 'unrest
      '((synonyms (agitation trepidation disquiet stir ferment foment disturbance commotion turmoil turbulence hubbub fuss row disorder))
       )
)
(mcon 'untruth
      '((synonyms (lie falsehood falsity prevarication fib fallacy))
	(antonyms (truth verity fact reality veracity truism))
	(plural (untruths))
       )
)
(mcon 'uphold
      '((synonyms (accommodate advocate aid assert assist back back-up boost champion defend encourage facilitate favor foster further help justify maintain patronize second stand-by strengthen support sustain vindicate))
	(tenses (upholds upheld upholding))
       )
)
(mcon 'upset
      '((synonyms (distraught perturbed troubled uneasy unsettled worried agitated discomposed disquieted disturbed flustered annoyed bothered irritated vexed))
	(antonyms (calm undisturbed unperturbed))
       )
)
(mcon 'use-for
      '((synonyms (use employ-for utilize-for apply-to exploit-for))
	(tenses (uses-for used-for using-for))
       )
)
(mcon 'vain
      '((synonyms (affected conceited narcissistic self-important snobbish pretentious proud))
	(antonyms (humble unpretentious))
       )
)
(mcon 'verse
      '((synonyms (poetry song rhyme))
        (antonyms (prose))
       )
)
(mcon 'very
      '((synonyms (exceedingly terribly really intensely))
	(antonyms (scarcely barely not-very hardly minutely))
       )
)
(mcon 'veterinary
      '((superordinates (doctor medic medical-practitioner physician))
	(synonyms (horse-doctor vet veterinarian))
	(plural (veterinaries))
       )
)
(mcon 'vicious
      '((synonyms (acrimonious base brutal cruel despicable evil ferocious fiendish fierce furious heinous malevolent malicious monstrous nasty nefarious sadistic venomous vile villainous violent wicked wild))
	(antonyms (benevolent benign kind merciful moral placid sensitive tender))
       )
)
(mcon 'victim
      '((superordinates (unfortunate-person))
        (subordinates (butt casualty dupe loser mark martyr prey quarry stooge))
        (synonyms (sufferer))
        (plural (victims))
       )
)
(mcon 'victim-of
      '((subordinates (butt-of casualty-of martyr-for prey-of quarry-of))
        (plural (victims-of))
       )
)
(mcon 'villain
      '((superordinates (bad-person))
	(subordinates (abductor cutthroat gangster gunman hit-man hood hoodlum kidnapper mugger murderer mutineer racketeer rapist robber thief thug))
	(synonyms (criminal crook felon lawbreaker outlaw))
	(plural (villains))
       )
)
(mcon 'violate
      '((synonyms (abuse assault attack breach break break-in debase defile deflower degrade desecrate dirty dishonor disobey disregard disturb fail-to-observe infringe interfere interfere-with interrupt outrage profane rape ravage ravish spoil sully tarn

ish transgress violate-sexually wrong))
	(tenses (violates violated violating))
       )
)
(mcon 'virtuous
      '((synonyms (chaste good innocent moral pure saintly upright))
	(antonyms (evil ignoble immoral unchaste wicked))
       )
)
(mcon 'visit
      '((synonyms (see call stay-with drop-in-on lodge-with))
	(tenses (visits visited visiting))
       )
)
(mcon 'vixen
      '((superordinates (fox female mammal))
	(plural (vixen))
       )
)
(mcon 'voice
      '((superordinates (sound))
	(synonyms (phrase speech))
	(part-of (animal human bird))
        (plural (voices))
       )
)
(mcon 'voice-of
      '((synonyms (phrase-of speech-of))
        (plural (voices-of))
       )
)
(mcon 'votaress
      '((superordinates (religious-person))
	(synonyms (worshipper believer devotee proselyte neophyte disciple))
	(plural (votaresses))
       )
)
(mcon 'vow
      '((synonyms (oath affirmation assertion declaration guarantee pledge promise))
	(plural (vows))
       )
)
(mcon 'vow-that
      '((synonyms (affirm assert asseverate aver avouch declare guarantee pledge promise state swear undertake vouch vow))
	(tenses (vows-that vowed-that vowing-that))
       )
)
(mcon 'vulture
      '((superordinates (bird))
	(subordinates (buzzard condor))
	(plural (vultures))
       )
)
) ; end of semantics_uv

(defun semantics_wxyz ()

(mcon 'wagon
      '((superordinates (vehicle))
	(subordinates (icewagon milkwagon))
	(part-of (wagon-train caravan))
	(sub-parts (wheel axle body))
	(plural (wagons))
       )
)
(mcon 'wait
      '((synonyms (abide await stay tarry))
	(antonyms (continue))
	(tenses (waits waited waiting))
       )
)
(mcon 'wait-for
      '((synonyms (wait await stay-for))
	(tenses (waits-for waited-for waiting-for))
       )
)
(mcon 'wait-until
      '((synonyms (wait await stay-until tarry-until))
	(tenses (waits-until waited-until waiting-until))
       )
)
(mcon 'wake
      '((synonyms (arouse rouse stir waken wake-up))
	(tenses (wakes woke waking))
       )
)
(mcon 'wake-up
      '((synonyms (arouse rouse stir wake waken))
	(tenses (wakes-up woke-up waking-up))
       )
)
(mcon 'walk
      '((synonyms (amble hike lumber march pace perambulate ramble roam saunter stroll wander))
	(tenses (walks walked walking))
       )
)
(mcon 'wall
      '((superordinates (impediment obstruction))
	(subordinates (divider panel partition))
	(sub-parts (beams wood nails stones mortar))
	(part-of (structure house building))
	(plural (walls))
       )
)
(mcon 'wander
      '((synonyms (deviate digress drift err fall go-wrong lapse lose-one meander ramble range range-over roam rove sin slip stray stroll stumble travel-randomly trip walk walk-aimlessly))
	(tenses (wanders wandered wandering))
       )
)
(mcon 'want
      '((synonyms (covet crave desire fancy))
	(tenses (wants wanted wanting))
       )
)
(mcon 'want-from
      '((synonyms (want desire-of wish-of need-from))
	(tenses (wants-from wanted-from wanting-from))
       )
)
(mcon 'wanton
      '((synonyms (dissipated dissolute easy gratuitous lewd licentious light loose low-minded needless rakish supererogatory uncalled-for unchaste unjustified unwarranted))
	(antonyms (chaste moral necessary sober))
	(plural (wantons))
       )
)
(mcon 'war
      '((synonyms (fight struggle))
	(antonyms (peace))
	(plural (wars))
       )
)
(mcon 'warlike
      '((synonyms (bellicose belligerent militant hostile))
	(antonyms (peaceful civil amicable))
       )
)
(mcon 'warm
      '((synonyms (lukewarm hot))
	(antonyms (cold cool))
       )
)
(mcon 'warm-up
      '((synonyms (warm heat))
	(tenses (warms-up warmed-up warming-up))
       )
)
(mcon 'warn
      '((synonyms (advise alert caution counsel))
	(tenses (warns warned warning))
       )
)
(mcon 'warn-of
      '((synonyms (warn advise-of alert-to caution-about))
	(tenses (warns-of warned-of warning-of))
       )
)
(mcon 'warn-against
      '((synonyms (warn forewarn caution counsel-against caution-against alert-about warn-about))
	(tenses (warns-against warned-against warning-against))
       )
)
; This is an ugly temporal patch.
(mcon 'was-false
      '((antonyms (was-true))
       )
)
(mcon 'wasp
      '((superordinates (insect))
	(subordinates (hornet yellow-jacket))
	(plural (wasps))
       )
)
(mcon 'waste
      '((synonyms (dissipate fritter fritter-away lay-waste-to spend squander))
	(tenses (wastes wasted wasting))
       )
)
(mcon 'watch
      '((synonyms (attend attend-to guard heed look look-at mark mind monitor observe see tend tend-to watch-over))
	(tenses (watches watched watching))
       )
)
(mcon 'water
      '((superordinates (substance beverage))
	(subordinates (ice-water mineral-water dew))
      )
)
(mcon 'wave
      '((synonyms (ripple))
	(part-of (ocean sea))
	(plural (waves))
       )
)
(mcon 'weak
      '((synonyms (defenseless delicate exposed faint feeble fragile frail gentle mild powerless tender vulnerable wobbly young))
	(antonyms (invulnerable potent powerful robust strong tough unconquerable))
       )
)
(mcon 'weaker
      '((superordinates (more weak))
	(synonyms (feebler milder))
	(antonyms (stronger tougher))
       )
)
(mcon 'weaker-than
      '((superordinates (more-than weak))
	(synonyms (feebler-than milder-than))
	(antonyms (stronger-than tougher-than))
       )
)
(mcon 'wear
      '((synonyms (bear don have-on))
	(tenses (wears wore wearing))
       )
)
(mcon 'weasel
      '((superordinates (mustelid mammal))
	(subordinates (ermine stoat))
	(plural (weasels))
       )
)
(mcon 'weaver
      '((superordinates (laborer craftsman))
	(plural (weavers))
       )
)
(mcon 'welcome
      '((synonyms (accept adopt greet host receive))
	(antonyms (ignore refuse reject turn-away))
	(tenses (welcomes welcomed welcoming))
       )
)
(mcon 'water-well
      '((superordinates (cistern water-tank))
	(sub-parts (bucket water))
	(plural (water-wells))
       )
)
; note: these are not really the meanings WENCH is used for in the plays.
;  the only useful meanings are woman, young woman, peasant or country-girl
; note: then why did you use WENCH, instead of something more appropriate?
(mcon 'wench
      '((superordinates (female female-person woman bad-person))
	(synonyms (adulteress bitch country-girl dame damsel fornicatress girl hussy jade loose-woman maid maiden miss slut strumpet trollop young-lady young-woman))
	(plural (wenches))
       )
)
(mcon 'whale
      '((superordinates (mammal sea-creature cetacean))
	(sub-parts (fins blubber baleen blow-hole tail))
	(subordinates (humpback-whale blue-whale killer-whale sperm-whale right-whale white-whale beluga-whale black-whale))
	(plural (whales))
       )
)
(mcon 'when
      '((synonyms (while as))
       )
)
(mcon 'while
      '((synonyms (whilst when))
	(antonyms (elsewhile elsewhen))
       )
)
(mcon 'wild
      '((synonyms (ferocious reckless savage untamed))
	(antonyms (civilized placid tame))
       )
)
(mcon 'will
      '((synonyms (bequest bequeathal legacy testament codicil probate inheritance decree))
	(plural (wills))
       )
)
(mcon 'win
      '((synonyms (prevail triumph succeed conquer))
	(tenses (wins won winning))
       )
)
(mcon 'wisdom
      '((synonyms (sageness sagacity intelligence prudence))
	(antonyms (youth inexperience rashness))
       )
)
(mcon 'wise
      '((synonyms (prudent sage knowledgeable sensible intelligent reasonable sagacious astute mature))
	(antonyms (unwise stupid inexperienced unintelligent immature))
       )
)
(mcon 'wiser
      '((superordinates (more wise))
       )
)
(mcon 'witch
      '((superordinates (woman female human-being supernatural))
	(synonyms (sorceress hag))
	(subordinates (Wierd-Sisters))
	(part-of (coven))
	(plural (witches))
       )
)
(mcon 'without
      '((synonyms (lacking failing))
	(antonyms (with having))
       )
)
(mcon 'wolf
      '((superordinates (canid mammal))
	(subordinates (timber-wolf))
	(plural (wolves))
       )
)
(mcon 'woman
      '((superordinates (human-being person adult female))
	(synonyms (gentlewoman lady madame))
	(antonyms (man))
	(plural (women))
       )
)
(mcon 'woo
      '((synonyms (court make-love))
	(tenses (woos wooed wooing))
       )
)
(mcon 'woods
      '((synonyms (wood woodland timberland timber forest jungle))
	(subordinates (forest-preserve state-forest national-forest rain-forest cloud-forest climax-forest sprout-forest selection-forest primeval-forest virgin-forest))
       )
)
(mcon 'worried
      '((synonyms (alarmed anxious apprehensive uneasy distraught purturbed troubled unquiet unsettled upset agitated disquieted afraid concerned apprehensive disturbed fearful))
	(antonyms (unafraid calm comfortable unconcerned confident))
       )
)
(mcon 'worse
      '((superordinates (more bad))
	(synonyms (inferior))
	(antonyms (better))
      )
)
(mcon 'worse-than
      '((superordinates (more-than bad))
	(synonyms (inferior-to))
	(antonyms (better-than))
       )
)
(mcon 'worship
      '((synonyms (adore revere venerate honor idolize))
	(tenses (worships worshiped worshiping))
       )
)
(mcon 'wound
      '((synonyms (injure hurt harm damage))
	(tenses (wounds wounded wounding))
       )
)
(mcon 'wounded
      '((synonyms (afflicted cut damaged hurt injured lacerated maimed mutilated rent torn))
	(antonyms (sound unharmed))
       )
)
(mcon 'wreck
      '((synonyms (blast break cause-to-founder crush damage defeat demolish destroy devastate dilapidate disable imperil lay-waste level pull-down ravage raze repair-wreckage ruin sabotage salvage scuttle shipwreck smash tear-down tumble undo waste wrack

))
	(tenses (wrecks wrecked wrecking))
       )
)
(mcon 'wrestle
      '((synonyms (battle combat conflict contend contest fight grapple tussle vie))
	(tenses (wrestles wrestled wrestling))
       )
)
(mcon 'wretched
      '((synonyms (desolate joyless miserable pitiful poor squalid))
	(antonyms (happy rich))
       )
)
(mcon 'write
      '((synonyms (communicate compose correspond create delineate-in-words design draft draw exchange-thoughts formulate gesticulate indite make-note-of note produce project record send-messages set-down speak take-down transmit write-down))
	(tenses (writes wrote writing))
       )
)
(mcon 'wrong
      '((synonyms (erroneous incorrect immoral untrue unjust false))
	(antonyms (right accurate correct just true virtuous))
       )
)
(mcon 'wrongdoing
      '((synonyms (vice evildoing misconduct misdemeanor malpractice sin crime))
	(antonyms (virtue probity good-deed))
       )
)
(mcon 'young
      '((synonyms (fresh immature juvenile prime vulnerable weak youthful))
	(antonyms (mature old senior strong tough))
       )
)
(mcon 'youth
      '((superordinates (human person))
	(subordinates (adolescent child kid teen-ager youngster))
	(synonyms (juvenile))
	(plural (youths))
       )
)
) ; end of semantics_wxyz

