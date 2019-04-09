

(defun semantics_a ()
(mcon 'abandon
      '((synonyms (abdicate abnegate betray break-off cast-off cede concede deceive decline desert discard dismiss ditch drop flee forgo forsake get-rid-of give-up junk lead-astray leave leave-behind mislead quit reject relinquish renounce resign scrap se

duce strand surrender throw-away throw-out vacate waive withdraw-from yield))
	(tenses (abandons abandoned abandonning))
       )
)
(mcon 'abdicate
      '((synonyms (resign vacate abnegate relinquish cede renounce))
	(tenses (abdicates abdicated abdicating))
       )
)		
(mcon 'abstention
      '((synonyms (abstinence abstemiousness refraining eschewal))
       )
)
(mcon 'abuse
      '((synonyms (mistreat hurt harm injure exploit))
	(tenses (abuses abused abusing))
       )
)
(mcon 'accept
      '((synonyms (recieve take allow concede acknowledge grant admit
		   bear endure suffer abide tolerate stand withstand
		   bide sustain acquire obtain get accept-from))
	(antonyms (deny refuse))
	(tenses (accepts accepted accepting))
       )
)
(mcon 'accept-from
      '((synonyms (recieve-from take-from acquire-from obtain-from get-from accept))
	(tenses (accepts-from accepted-from accepting-from))
       )
)
(mcon 'accident
      '((synonyms (misfortune mishap misadventure mischance disaster
		   calamity catastrophe cataclysm tragedy bad-luck))
	(plural (accidents))
       )
)
(mcon 'accompany
      '((synonyms (attend escort convoy guard))
	(tenses (accompanies accompanied accompanying))
       )
)
(mcon 'accomplish
      '((synonyms (achieve attain complete consummate do effect execute implement perform realize stage swing))
	(antonyms (undone))
	(tenses (accomplishes accomplished accomplishing))
       )
)
(mcon 'accuse
      '((synonyms (arraign ascribe blame call-to-account charge complain criminate denounce hold-accountable-for impeach incriminate indict indight lay-before-a-court name present question tax))
	(tenses (accuses accused accusing))
       )
)
(mcon 'accuse-of
      '((synonyms (accuse arraign-for ascribe-to blame-for call-to-account-for charge-of complain criminate denounce-for hold-accountable-for impeach incriminate indict-for indight-for lay-before-a-court name present question tax))
	(tenses (accuses-of accused-of accusing-of))
       )
)
(mcon 'act
      '((synonyms (action))
	(plural (acts))
       )
)
(mcon 'actor
      '((superordinates (artist))
	(subordinates (barnstormer ham performer mime mimer mummer))
	(synonyms (actress histrion player thespian))
	(plural (actors))
       )
)
(mcon 'adrift
      '((synonyms (aimless directionless disoriented drifting planless unguided floating at-sea up-the-river-without-a-paddle))
	(antonyms (aimed purposeful stable))
       )
)
(mcon 'advantage
      '((synonyms (profit benefit))
	(plural (advantages))
       )
)
(mcon 'adventure
      '((superordinates (act))
	(synonyms (exploit enterprise heroic-act))
	(plural (adventures))
       )
)
(mcon 'advice
      '((synonyms (counsel recommendation suggestion instruction exhortation))
       )
)
(mcon 'afraid
      '((synonyms (fearful dreading alarmed frightened apprehensive
		   anxious uneasy apprehensive scared terrified 
		   worried))
	(antonyms (unafraid confident secure))
       )
)
(mcon 'afraid-of
      '((synonyms (afraid fearful-of frightened-of scared-of terrified-of))
	(antonyms (comfortable-with))
       )
)
(mcon 'after
      '((synonyms (following subsequent))
	(antonyms (anterior))
       )
)
(mcon 'afternoon
      '((superordinates (time))
	(part-of (day))
	(plural (afternoons))
       )
)
(mcon 'agents
      '((superordinates (official bad-person military-personnel))
	(synonyms (agency cause spy hired-assassin hit-man))
       )
)
(mcon 'agree
      '((synonyms (accede accord acquiesce arrange assent bargain befit belong check coincide come-to-agreement come-to-terms compare compromise concede concur conform consent correspond dispose fit harmonize jibe match negotiate settle sort-with square s

ubscribe suit tally yield))
	(tenses (agrees agreed agreeing))
       )
)
(mcon 'agree-upon
      '((synonyms (agree concur-on assent-to consent-to agree-to agree-on come-to-terms compromise))
	(tenses (agrees-upon agreed-upon agreeing-upon))
       )
)
(mcon 'aim-at
      '((synonyms (aim attempt seek direct-at point-at shoot-for drive-at))
	(tenses (aims-at aimed-at aiming-at))
       )
)
(mcon 'alive
      '((synonyms (living animate))
	(antonyms (dead inanimate))
       )
)
(mcon 'allow
      '((synonyms (admit approve approve-of authorize consent_to endorse give_leave let permit tolerate))
	(tenses (allows allowed allowing))
       )
)
(mcon 'allowed
      '((synonyms (permitted vouchsafed))
	(antonyms (impermissible))
       )
)
; Note: what are we going to do about things that are both nouns and verbs?
;  should we enter all of the information and then use a "syntax" field to
;  explain how it is used either way.
(mcon 'ally
      '((synonyms (friend confederate))
	(superordinates (associate))
	(antonyms (enemy))
	(plural (allies))
       )
)
(mcon 'ally-with
      '((synonyms (ally join-with join-forces-with cooperate-with))
	(tenses (allies-with allied-with allying-with))
       )
)
(mcon 'almost
      '((synonyms (close))
	(antonyms (far))
       )
)
(mcon 'along
      '((synonyms (on by-way-of))
       )
)
(mcon 'altered
      '((synonyms (adjusted changed denatured modified))
	(antonyms (natural same unchanged unmodified unvaried))
       )
)
(mcon 'always
      '((synonyms (constantly regularly incessantly ceaselessly perpetually))
	(antonyms (infrequently seldom rarely sparsely))
       )
)
(mcon 'ambush
      '((synonyms (hiding hiding_place))
	(plural (ambushes))
       )
)
(mcon 'anger
      '((superordinates (negative_emotion))
	(synonyms (acerbity annoyance bitterness choler displeasure exasperation frustration fury ire irritation malevolence perturbation petulance rage resentment spite spleen venomousness vexation wrath))
       )
)
(mcon 'angry
      '((synonyms (angered enraged exasperated fiery furious heated irate ireful mad raging red-hot wrathful wroth))
	(antonyms (cold passionless placated placid pleased unemotional))
       )
)
(mcon 'animal
      '((superordinates (organism living-thing))
	(subordinates (mammal primate reptile amphibian fish bird
		       insect vertebrate invertebrate game stock))
	(synonyms (beast creature fauna))
	(antonyms (plant flora))
	(plural (animals))
       )
)
(mcon 'antidote
      '((synonyms (curative cure remedy))
	(plural (antidotes))
       )
)
(mcon 'antler
      '((part-of (animal))
	(plural (antlers))
       )
)
(mcon 'antler-of
      '((synonyms (antler))
	(plural (antlers-of))
       )
)
(mcon 'ape
      '((superordinates (simian mammal))
	(subordinates (chimp chimpanzee gorilla orangutan))
	(plural (apes))
       )
)
(mcon 'apologetic
      '((synonyms (sorry remorseful apologizing repentant regretful))
	(antonyms (unrepentant impenitent))
       )
)
(mcon 'apologize
      '((synonyms (repent ask-forgiveness beg-pardon))
	(tenses (apologizes apologized apologizing))
       )
)
(mcon 'appear
      '((synonyms (be-visible emerge look look-like seem show-up unfold))
	(tenses (appears appeared appearing))
       )
)
(mcon 'appear-that
      '((synonyms (appear emerge-that seem-that unfold-that))
	(tenses (appears-that appeared-that appearing-that))
       )
)
(mcon 'appearance
      '((synonyms (look looks))
	(plural (appearances))
       )
)
(mcon 'appearance-of
      '((synonyms (appearance looks-of))
	(plural (appearances-of))
       )
)
(mcon 'applaud
      '((synonyms (clap commend extol praise))
	(tenses (applauds applauded applauding))
       )
)
(mcon 'applaud-for
      '((synonyms (applaud clap-for praise))
	(tenses (applauds-for applauded-for applauding-for))
       )
)
(mcon 'approach
      '((synonyms (draw_near reach touch verge come-close approximate))
       )
)
(mcon 'argue
      '((synonyms (dispute disagree quarrel))
	(antonyms (agree))
	(tenses (argues argued arguing))
       )
)
(mcon 'argue-about
      '((synonyms (argue dispute debate quarrel-about))
	(antonyms (agree-upon))
	(tenses (argues-about argued-about arguing-about))
       )
)
(mcon 'army
      '((synonyms (militia military forces troops host legions))
	(plural (armies))
       )
)
(mcon 'arrest
      '((synonyms (apprehend attach attract block capture catch check delay detain engage enlist grab halt hinder hold hold-back hold-up interest interrupt keep keep-back nab pinch prevent pull-in restrain seize slow-down snub stay stop stop-for-a-time ta

ke take-in take-into-custody take-prisoner))
	(tenses (arrests arrested arresting))
       )
)
(mcon 'arrive
      '((synonyms (appear arrive-at come come-to gain get-to hit make reach show show-up turn-up))
	(tenses (arrives arrived arriving))
       )
)
(mcon 'arrive-at
      '((synonyms (arrive attain come_to reach))
	(tenses (arrives-at arrived-at arriving-at))
       )
)
(mcon 'arrow
      '((superordinates (projectile missile weapon arm))
	(synonyms (crossbow-bolt))
	(part-of (bow quiver))
	(plural (arrows))
       )
)
; This is ugly.  I couldn't even find it in the Thesaurus.
(mcon 'as
      '((synonyms (while since))
       )
)
(mcon 'ashamed
      '((synonyms (abashed embarrassed humiliated))
       )
)
(mcon 'ask
      '((synonyms (inquire interrogate query question request))
	(tenses (asks asked asking))
       )
)
(mcon 'ask-if
      '((synonyms (ask ask-whether inquire-if))
	(tenses (asks-if asked-if asking-if))
       )
)
(mcon 'ask-what
      '((synonyms (ask inquire-what))
	(tenses (asks-what asked-what asking-what))
       )
)
(mcon 'ask-why
      '((synonyms (ask question inquire-why))
	(tenses (asks-why asked-why asking-why))
       )
)
(mcon 'asleep
      '((synonyms (sleeping resting reposing))
	(antonyms (awake))
       )
)
(mcon 'ass
      '((superordinates (equid equine mammal))
	(subordinates (burro donkey))
	(synonyms (jackass jenny dunce dummy fool idiot cretin jerk))
	(plural (asses))
       )
)
(mcon 'assail
      '((synonyms (assault attack beset hit strike))
	(tenses (assails assailed assailing))
       )
)
(mcon 'assailant
      '((superordinates (bad-person))
	(subordinates (bully cutthroat mugger murderer rapist robber ruffian slayer))
	(synonyms (aggressor attacker))
	(plural (assailants))
       )
)
(mcon 'assassinate
      '((synonyms (dispatch execute kill murder put-to-death remove slay))
	(tenses (assassinates assassinated assassinating))
       )
)
(mcon 'assembly
      '((synonyms (gathering congregation meeting conclave convention))
	(plural (assemblies))
       )
)
(mcon 'assembly-of
      '((synonyms (assembly gathering-of congregation-of meeting-of conclave-of convention-of))
	(plural (assemblies-of))
       )
)
(mcon 'assist
      '((synonyms (abet accommodate aid back-up boost collaborate compliment contribute cooperate do-one encourage facilitate favor foster further give-military-support help patronize play-along protect second share supply support sustain uphold))
	(tenses (assists assisted assisting))
       )
)
(mcon 'assistant
      '((synonyms (helper supporter))
	(superordinates (worker))
	(subordinates (adjutant aide aide-de-camp attache deputy 
		       lieutenant subordinate auxiliary amanuensis 
		       clerk copyist scribe scrivener attendant 
		       escort guidance guide squire usher butler 
		       flunky gentleman lackey
		       manservant valet inferior))
       )
)
(mcon 'at
      '((synonyms (in on by near next-to))
       )
)
(mcon 'at-war-with
      '((synonyms (war warring fighting fight-with))
       )
)
(mcon 'attach
      '((synonyms (add add-on adhere adjoin affix anchor annex append ascribe assign associate assume-jurisdiction attribute combine connect couple fasten fix garnish glue grip hang hitch hold impound impute include incorporate join link make-fast nail pi

n pull-in reassign-temporarily secure seize staple stay stick subjoin suspend tack tack-on take take-by-legal-authority take-in take-into-custody take-possession-of tether tie unite))
	(tenses (attaches attached attaching))
       )
)
(mcon 'attack
      '((synonyms (injure invade assault strike raid))
	(tenses (attacks attacked attacking))
       )
)
(mcon 'attempt
      '((synonyms (effort endeavor struggle))
	(plural (attempts))
       )
)
(mcon 'attempt-to
      '((synonyms (attempt endeavor-to strive-to try-to undertake-to))
	(tenses (attempts-to attempted-to attempting-to))
       )
)
(mcon 'attendant
      '((superordinates (assistant helper supporter))
	(synonyms (accompanying affiliated allied appendant associated escort guidance guide squire usher))
	(plural (attendants))
       )
)
(mcon 'avarice
      '((superordinates (desire))
	(synonyms (greed selfishness miserliness gluttony stinginess))
	(antonyms (selflessness abstemic generosity unselfishness))
       )
)
(mcon 'avenge
      '((synonyms (get_even pay_back retaliate revenge vindicate))
	(tenses (avenges avenged avenging))
       )
)
(mcon 'avenged
      '((synonyms (repaid vindicated made-right paid-back satisfied
		   gotten-even))
       )
)
(mcon 'avoid
      '((synonyms (evade shun elude escape circumvent))
	(tenses (avoids avoided avoiding))
       )
)
(mcon 'awaken
      '((synonyms (rouse stir awake))
	(variations (wake-up))
	(tenses (awakens awakened awakening))
       )
)
) ;end of semantics_a

(defun semantics_b ()

(mcon 'bachelor
      '((synonyms (maiden))
	(antonyms (married))
	(plural (bachelors))
       )
)
(mcon 'backtrack
      '((synonyms (reverse backtrail countermarch retreat))
	(tenses (backtracks backtracked backtracking))
       )
)
(mcon 'bad
      '((synonyms (foul evil wicked fake poor inferior disobedient))
	(antonyms (good genuine moral obedient sound virtuous))
       )
)
(mcon 'bad-for
      '((synonyms (bad unhealthy-for))
	(antonyms (good-for))
       )
)
(mcon 'badly
      '((synonyms (poorly despicably basely ruinously gravely terribly deficiently imperfectly unsoundly incorrectly))
	(antonyms (well soundly correctly))
       )
)
(mcon 'bait
      '((synonyms (decoy lure))
	(part-of (trap))
	(plural (baits))
       )
)
(mcon 'bandit
      '((synonyms (thief robber))
	(plural (bandits))
       )
)
(mcon 'banish
      '((synonyms (exile expel oust))
	(tenses (banishes banished banishing))
       )
)
(mcon 'banished
      '((synonyms (expatriated deported exiled expelled ostracized
		   cast-out dispossessed ejected ousted driven-out))
       )
)
(mcon 'barely
      '((synonyms (scarcely hardly slightly scantily minimally))
	(antonyms (greatly largely much considerably))
       )
)
(mcon 'barley
      '((superordinates (cereal grain))
       )
)
(mcon 'bastard
      '((superordinates (unfortunate-person child))
	(synonyms (illegitimate))
	(antonyms (legitimate))
       )
)
; How do we distinguish baseball-bat from vampire-bat?
(mcon 'bat
      '((superordinates (club cudgel mammal racket sports_equipment))
	(subordinates (fruit_bat vampire))
	(synonyms (baseball_bat))
	(plural (bats))
       )
)
(mcon 'battle
      '((synonyms (fight combat))
	(superordinates (contest))
	(part-of (war))
	(plural (battles))
       )
)
(mcon 'beam
      '((superordinates (wood))
	(synonyms (ray timber))
	(plural (beams))
       )
)
(mcon 'bear
      '((superordinates (fissiped mammal))
	(subordinates (grizzly polar-bear))
	(plural (bears))
       )
)
(mcon 'beast
      '((superordinates (living_thing organism))
	(subordinates (beast_of_burden game mammal prey quarry stock))
	(synonyms (animal being brute creature fauna))
	(sub-parts (carcass frame head limb semen skeleton skin))
	(plural (beasts))
       )
)
(mcon 'beautiful
      '((synonyms (aesthetic attractive comely fair lovely pretty gorgeous exquisite striking))
	(antonyms (ugly unattractive homely hideous))
       )
)
(mcon 'become-false
      '((antonyms (become-false))
       )
)
(mcon 'become-true
      '((antonyms (become-false))
       )
)
(mcon 'befall
      '((synonyms (happen happen-to occur transpire))
	(tenses (befalls befell befalling))
       )
)
(mcon 'before
      '((synonyms (preceeding prior-to ahead-of))
	(antonyms (after behind))
       )
)
(mcon 'befriend
      '((antonyms (alienate estrange))
	(tenses (befriends befriended befriending))
       )
)
(mcon 'beg
      '((synonyms (ask beseech entreat implore importune plead))
	(tenses (begs begged begging))
       )
)
(mcon 'beg-that
      '((synonyms (beg ask-that entreat-that plead-that))
	(tenses (begs-that begged-that begging-that))
       )
)
(mcon 'beg-to
      '((synonyms (beg ask-to beseech-to entreat-to implore-to))
	(tenses (begs-to begged-to begging-to))
       )
)
(mcon 'beget
      '((synonyms (bear breed conceive engender hatch have procreate sire))
	(tenses (begets begot begetting))
       )
)
(mcon 'begin
      '((synonyms (commence emerge erupt initiate originate start))
	(tenses (begins began beginning))
       )
)
(mcon 'behind
      '((synonyms (rearward hidden))
	(antonyms (in-front-of forward visible before))
       )
)
(mcon 'believe
      '((synonyms (think suppose reckon figure assume fancy
		   imagine buy accept conjecture feel consider
		   trust credit))
	(tenses (believes believed believing))
       )
)
(mcon 'belong-to
      '((superordinates (possess))
	(tenses (belongs-to belonged-to belonging-to))
       )
)
(mcon 'benefactor
      '((superordinates (defender guardian protector))
	(synonyms (patron protector sponsor supporter))
	(plural (benefactors))
       )
)
(mcon 'benefit
      '((synonyms (advance aid further help improve serve))
	(tenses (benefits benefitted benefitting))
       )
)
(mcon 'benevolent
      '((synonyms (beneficent charitable generous gracious helpful kind kindly loving philanthropic))
	(antonyms (austere evil hostile inhumane malevolent stingy uncharitable unkind))
       )
)
(mcon 'bent-on
      '((synonyms (bound determined resolved-to set-on))
	(antonyms (disinclined-to))
       )
)
(mcon 'beseige
      '((superordinates (attack))
	(synonyms (lay-seige-to blockade beset beleaguer harry harass))
       )
)
(mcon 'betray
      '((synonyms (abandon beguile deceive delude desert double-cross mislead))
	(tenses (betrays betrayed betraying))
       )
)
(mcon 'betray-to
      '((synonyms (betray disclose-to divulge-to expose-to report-to reveal-to sell-to uncover-to))
	(antonyms (remain-loyal keep-confidence))
	(tenses (betrays-to betrayed-to betraying-to))
       )
)
(mcon 'betrothed
      '((synonyms (engaged affianced betrothed intended))
       )
)
; Obviously, BIGGER and BETTER should have rules which specify how they
; relate to MORE, BIG, and GOOD.  For now, the superordinates will be used
; to get this information in there.
(mcon 'better
      '((superordinates (more good))
	(synonyms (superior))
	(antonyms (worse inferior))
       )
)
(mcon 'big
      '((synonyms (large huge tremendous enormous))
	(antonyms (little short small))
       )
)
(mcon 'bigger
      '((superordinates (more big))
	(synonyms (larger greater))
	(antonyms (smaller))
       )
)
; Should BIRD be this detailed?  More?
(mcon 'bird
      '((superordinates (animal))
	(subordinates (duck goose hawk dove pigeon turkey eagle nightingale sparrow eagle buzzard hawk vulture))
	(synonyms (fowl))
	(sub-parts (beak feather wing claw talon))
	(plural (birds))
      )
)
(mcon 'birdcatcher
      '((superordinates (animal-tending-person))
	(plural (birdcatchers))
       )
)
(mcon 'bishop
      '((superordinates (religious-person clergyman))
	(plural (bishops))
       )
)
(mcon 'bite
      '((synonyms (chew eat_into gnaw hurt nip snap))
	(tenses (bites bit biting))
       )
)
(mcon 'bitter
      '((synonyms (acid acidulous acrid acrimonious afflicting atrocious biting burning caustic condemnatory corrosive cutting dire distasteful distressing dolorous grievous harsh hurtful incensed indignant injurious keen lamentable outraged penetrating p

iercing rancorous raw resentful revengeful sharp sore spiteful stinging stout tart unpalatable unsavory vengeful vicious vindictive virulent vitriolic wrathful wroth))
	(antonyms (amicable benevolent bland hot lovable painless palatable placated pleasant soft sweet tasteless))
       )
)
(mcon 'blame
      '((synonyms (accuse charge reproach condemn reprove reprehend))
	(tenses (blames blamed blaming))
       )
)
(mcon 'blame-for
      '((synonyms (blame accuse-of reproach-for condemn-for reprove-for))
	(tenses (blames-for blamed-for blaming-for))
       )
)
(mcon 'blessing
      '((synonyms (approval sanction favor acceptance endorsement benediction))
	(antonyms (curse damnation disapproval disapprobation disfavor))
	(plural (blessings))
       )
)
(mcon 'blessing-of
      '((synonyms (blessing approval-of sanction-of favor-of acceptance-of endorsement-of benediction-of))
	(antonyms (curse-of damnation-of disapproval-of disfavor-of))
	(plural (blessings-of))
       )
)
(mcon 'blind
      '((synonyms (sightless unseeing))
	(antonyms (seeing))
       )
)
(mcon 'boar
      '((superordinates (pig mammal))
	(plural (boars))
       )
)
(mcon 'boast
      '((superordinates (speech-act claim statement))
	(synonyms (brag))
	(plural (boasts))
       )
)
(mcon 'bone
      '((superordinates (body-part connective-tissue animal-product))
	(subordinates (horn ivory tusk whalebone))
	(part-of (skeleton))
	(plural (bones))
       )
)
(mcon 'borrow
      '((synonyms (take-out-a-loan adopt collect cop filch lift nab pick pilfer pinch purloin round-up scavange scrounge sneak snitch steal swipe take take-over))
	(tenses (borrows borrowed borrowing))
       )
)
; Then, of course, there's bow (with arrows) and bow (violin) and
; bow (ribbon) and bow (down) and bow (of a ship)
(mcon 'bow
      '((superordinates (weapon arm adornment decoration knot))
	(synonyms (ribbon crossbow violin-bow))
	(subordinates (crossbow))
	(sub-parts (arrows))
	(part-of (garment))
	(plural (bows))
       )
)
(mcon 'boy
      '((superordinates (child kid male person youngster))
	(synonyms (cub lad male-child school-boy))
	(plural (boys))
       )
)
(mcon 'bracelet
      '((superordinates (adornment jewelry))
	(subordinates (anklet ankle-bracelet))
	(synonyms (bangle))
	(plural (bracelets))
       )
)
(mcon 'brag
      '((synonyms (boast exaggeration))
	(antonyms (truth))
	(plural (brags))
       )
)
(mcon 'brag-about
      '((synonyms (brag boast-about exaggerate-about))
	(tenses (brags-to bragged-to bragging-to))
       )
)
(mcon 'brag-to
      '((synonyms (brag boast-to exaggerate-to))
	(tenses (brags-to bragged-to bragging-to))
       )
)
(mcon 'braggart
      '((superordinates (unpleasant-person))
	(synonyms (boaster brag braggadocio))
       )
)
(mcon 'break
      '((superordinates (damage))
	(synonyms (fracture crack burst bust snap rupture shatter
		   split smash splinter shiver dash crush part
		   separate rend fragment wreck demolish breach))
	(tenses (breaks broke breaking))
       )
)
(mcon 'break-into
      '((synonyms (get-into break-open open))
	(tenses (breaks-into broke-into breaking-into))
       )
)
(mcon 'bridle
      '((superordinates (restraint))
	(synonyms (halter))
	(plural (bridles))
       )
)
(mcon 'bring
      '((synonyms (carry convey deliver lead))
	(tenses (brings brought bringing))
       )
)
(mcon 'bring-to
      '((synonyms (bring convey-to deliver-to lead-to))
	(tenses (brings-to brought-to bringing-to))
       )
)
(mcon 'broken
      '((synonyms (inoperable inoperative out_of_order unserviceable
		   chipped cracked damaged fractured shattered 
		   smashed nonfunctional))
	(antonyms (unbroken functional unharmed perfect sound))
       )
)
(mcon 'brothel
      '((synonyms (house-of-prostitution whorehouse cathouse bordello den-of-iniquity))
       )
)
(mcon 'brother
      '((superordinates (man relation kin sibling))
       )
)
(mcon 'bull
      '((superordinates (bovid mammal))
	(synonyms (bullock))
	(plural (bulls))
       )
)
(mcon 'bullock
      '((superordinates (cattle kine neat))
	(synonyms (bull))
	(plural (bullocks))
       )
)
(mcon 'burn
      '((synonyms (blaze char parch scald scorch sear singe))
	(tenses (burns burned burnt burning))
       )
)
(mcon 'burning
      '((synonyms (afire aflame blazing flaming scorching searing sizzling))
	(antonyms (cold))
       )
)
(mcon 'burst
      '((synonyms (blow-up break bust crack explode fracture rupture rush shatter))
	(tenses (bursts burst bursting))
       )
)
(mcon 'bush
      '((superordinates (plant flora))
	(synonyms (hedge shrub))
	(part-of (hedge))
	(plural (bushes))
       )
)
(mcon 'business
      '((synonyms (industry trade))
	(plural (businesses))
       )
)
(mcon 'butt-against
      '((synonyms (hit buffet bump))
	(tenses (butts-against butted-against butting-against))
       )
)
(mcon 'buy
      '((synonyms (purchase trade))
	(tenses (buys bought buying))
       )
)
) ;end of semantics_b

(defun semantics_c ()

(mcon 'cage
      '((superordinates (enclosure))
	(subordinates (birdcage))
	(synonyms (coop corral enclosure pen))
	(plural (cages))
       )
)
(mcon 'call-together
      '((synonyms (assemble bring-together))
	(tenses (calls-together called-together called-together))
       )
)
(mcon 'camel
      '((superordinates (mammal))
	(subordinates (arabian-camel bactrian-camel dromedary))
	(plural (camels))
       )
)
(mcon 'campaign
      '((subordinates (political-campaign))
	(synonyms (competition race solicitation))
	(plural (campaigns))
       )
)
(mcon 'cannot
      '((synonyms (impossible not-possible incapable non-capable))
	(antonyms (can possible capable))
       )
)
(mcon 'capitulate
      '((synonyms (defer give-in give-up relent submit succumb surrender yield))
	(tenses (capitulates capitulated capitulating))
       )
)
(mcon 'capsize
      '((synonyms (overturn tip turn-over upset))
	(tenses (capsizes capsized capsizing))
       )
)
(mcon 'captain
      '((superordinates (leader))
	(synonyms (head-waiter maitre-d senior-pilot head chief commander commanding-officer skipper))
	(plural (captains))
       )
)
(mcon 'captive
      '((synonyms (confined imprisoned))
	(antonyms (free))
       )
)
(mcon 'capture
      '((synonyms (catch conquer ensnare entrap grab hold seize take take-over trap occupy subdue take-prisoner))
	(tenses (captures captured capturing))
       )
)
(mcon 'care-for
      '((synonyms (care dote-on guard help like look-after love mind nurse nurture provide-for tend watch watch-over))
	(tenses (cares-for cared-for caring-for))
       )
)
(mcon 'carry
      '((synonyms (bear convey deliver hold lug take transport))
	(tenses (carries carried carrying))
       )
)
(mcon 'casket
      '((superordinates (box))
	(synonyms (coffin sarcophagus))
	(plural (caskets))
       )
)
(mcon 'cast
      '((synonyms (fling flip heave hurl pitch shoot throw toss))
	(tenses (casts cast casting))
       )
)
(mcon 'castaway
      '((superordinates (unfortunate-person))
	(synonyms (abandoned-person shipwreck-survivor))
	(plural (castaways))
       )
)
(mcon 'cast-into
      '((synonyms (cast fling-into flip-into heave-into hurl-into pitch-into put-into shoot-into throw-into toss-into))
	(tenses (casts-into cast-into casting-into))
       )
)
(mcon 'cat
      '((superordinates (feline mammal))
	(subordinates (abyssinian angora big-cat house-cat kitten kitty manx mouser pussy pussycat siamese tabby tomcat wildcat))
	(synonyms (feline house-cat))
	(plural (cats))
       )
)
(mcon 'catch
      '((synonyms (block capture ensnare entrap halt hold nab seize
		   snare trap))
	(tenses (catches caught catching))
       )
)
(mcon 'catch-in
      '((synonyms (catch ensnare-in entrap-in snare-in trap-in catch-with))
	(tenses (catches-in caught-in catching-in))
       )
)
(mcon 'cause
      '((synonyms (bring-about bring-on effect induce lead-to))
	(tenses (causes caused causing))
       )
)
(mcon 'cautious
      '((synonyms (careful prudent))
	(antonyms (adventurous careless imprudent impulsive))
       )
)
(mcon 'cave
      '((superordinates (concavity geological-formation))
	(synonyms (cavern grotto burrow))
	(plural (caves))
       )
)
(mcon 'celebrate
      '((synonyms (commemorate extol feast fete honor observe proclaim))
	(tenses (celebrates celebrated celebrating))
       )
)
(mcon 'chaff
      '((superordinates (solid))
	(synonyms (husks stalks straw stubble))
       )
)
(mcon 'center
      '((synonyms (middle midpoint inside))
       )
)
(mcon 'ceremony
      '((synonym (rite ritual formality celebration festivity))
	(plural (ceremonies))
       )
)
(mcon 'challenge
      '((synonyms (arouse beard brave call-for confront contest dare defy demand deny dispute doubt excite face gainsay impugn provoke question require stimulate taunt))
	(tenses (challenges challenged challenging))
       )
)
(mcon 'champion
      '((superordinates (adherent competitor contender contestant defender follower guardian player protector))
	(synonyms (admirer advocate apologist back booster defend enthusiast fan favor fighter help hero paladin partisan partizan patron patronize protect protector support supporter uphold victor vindicator warrior winner))
	(plural (champions))
       )
)
(mcon 'change
      '((synonyms (alter modify influence revise convert transform))
	(tenses (changes changed changing))
       )
)
(mcon 'changed
      '((synonyms (adjusted altered denatured different modified))
	(antonyms (natural same unchanged unmodified unvaried))
       )
)
(mcon 'character
      '((synonyms (personage personality self))
	(part-of (human-being people person human))
	(plural (characters))
       )
)
(mcon 'character-of
      '((synonyms (character personage-of personality-of))
	(part-of (human-being people person human))
	(plural (characters-of))
       )
)
(mcon 'chase
      '((synonyms (pursue follow hunt))
	(tenses (chases chased chasing))
       )
)
(mcon 'chase-away
      '((synonyms (chase drive-out drive-away))
	(tenses (chases-away chased-away chasing-away))
       )
)
(mcon 'chatter
      '((synonyms (chat prate prattle speak-idly talk tattle))
	(tenses (chatters chattered chattering))
       )
)
(mcon 'child
      '((superordinates (human animal young-person))
	(subordinates (boy girl))
	(synonyms (kid youngster))
	(plural (children))
       )
)
(mcon 'child-of
      '((synonyms (child))
	(plural (children-of))
       )
)
(mcon 'chip
      '((synonyms (crack splinter break fracture mutilate gash))
	(tenses (chips chipped chipping))
       )
)
(mcon 'choose
      '((synonyms (decide select pick prefer elect))
	(tenses (chooses chose choosing))
       )
)
(mcon 'circumstance
      '((synonyms (occurence occasion environment condition event))
	(plural (circumstances))
       )
)
; Note: there may be a reason to specify citizen as free-man or even to
;   make explicit the Roman definition of the term (as it has so far only
;   arisen in Julius Caesar).
(mcon 'citizen
      '((superordinates (normal-person))
	(synonyms (national subject native))
	(antonyms (slave foreigner alien))
	(part-of (population))
	(plural (citizens))
       )
)
(mcon 'city
      '((synonyms (town settlement village))
	(plural (cities))
       )
)
(mcon 'claw
      '((synonyms (nail talon))
	(part-of (animal))
	(plural (claws))
       )
)
(mcon 'claw-of
      '((synonyms (claw nail-of talon-of))
	(part-of (animal))
	(plural (claws-of))
       )
)
(mcon 'clean
      '((synonyms (brush cleanse refresh remove-dirt scour scrub tidy wash))
	(antonyms (dirty))
	(tenses (cleans cleaned cleaning))
       )
)
(mcon 'clean-in
      '((synonyms (clean cleanse-in wash-in))
	(tenses (cleans-in cleaned-in cleaning-in))
       )
)
(mcon 'clear
      '((synonyms (bright certain clean evident fair intelligible lucid obvious plain simple transparent unambiguous))
	(antonyms (ambiguous cloudy dim opaque unclear vague))
       )
)
(mcon 'cliff
      '((synonyms (bluff precipice wall crag escarpment))
	(plural (cliffs))
       )
)
(mcon 'climb-on
      '((synonyms (bestride get-on mount))
	(tenses (climbs-on climbed-on climbing-on))
       )
)
(mcon 'clown
      '((superordinates (entertainer))
	(synonyms (buffoon fool jester comic wit joker quipster prankster comedian))
       )
)
(mcon 'clumsy
      '((synonyms (awkward bumbling bungling graceless inept))
	(antonyms (adroit graceful))
       )
)
(mcon 'clumsily
      '((synonyms (awkwardly gracelessly ineptly))
	(antonyms (adroitly gracefully))
       )
)
(mcon 'cock
      '((superordinates (chicken domestic-fowl bird poultry))
	(synonyms (rooster))
	(plural (cocks))
       )
)
(mcon 'collapse
      '((synonyms (break buckle cave-in crumble disintegrate fall-in topple tumble yield))
	(tenses (collapses collapsed collapsing))
       )
)
(mcon 'collect
      '((synonyms (accumulate amass gather hoard stockpile store))
	(tenses (collects collected collecting))
       )
)
(mcon 'come-out
      '((synonyms (come come-forth emerge go-out issue))
	(tenses (comes-out came-out coming-out))
       )
)
(mcon 'come-to
      '((synonyms (come arrive go-to reach))
	(tenses (comes-to came-to coming-to))
       )
)
(mcon 'comfort
      '((superordinates (happiness))
	(synonyms (cheer console encourage gladden reassure relieve soothe))
	(tenses (comforts comforted comforting))
       )
)
(mcon 'commander
      '((superordinates (officer superior military-personnel))
	(synonyms (chief leader commandante))
	(plural (commanders))
       )
)
(mcon 'commit
      '((synonyms (carry-out do perform))
	(tenses (commits committed committing))
       )
)
(mcon 'compassionate
      '((synonyms (caring considerate feeling humane kind merciful sensitive sympathetic understanding warm))
	(antonyms (coldhearted inhumane insensitive merciless uncompassionate unkind unsympathetic))
       )
)
(mcon 'compete
      '((synonyms (conflict contend fight oppose rival vie))
	(tenses (competes competed competing))
       )
)
(mcon 'compete-with
      '((synonyms (compete conflict-with oppose))
	(antonyms (agree-with))
	(tenses (competes-with competed-with competing-with))
       )
)
(mcon 'complain
      '((synonyms (bewail bicker carp fuss gripe grumble lament moan object protest whine))
	(tenses (complains complained complaining))
       )
)
(mcon 'complain-to
      '((synonyms (complain grumble-to moan-to protest-to whine-at))
	(tenses (complains-to complained-to complaining-to))
       )
)
(mcon 'compose
      '((synonyms (adapt arrange calm constitute create design fashion form formulate integrate make-coherent make-up orchestrate organize pacify produce put-together quiet restore-tranquility score settle still subdue unify write))
	(tenses (composes composed composing))
       )
)
(mcon 'computer
      '((superordinates (machine calculating-device))
	(plural (computers))
       )
)
(mcon 'concern
      '((superordinates (confidence fear sympathy))
	(synonyms (bear-on be-about bother consume distress interest involve matter-to relate-to))
	(tenses (concerns concerned concerning))
       )
)
(mcon 'confess
      '((synonyms (acknowledge admit attest avouch avow concede declare disclose divulge make-known own profess prove reveal shrive tell utter))
	(tenses (confesses confessed confessing))
       )
)
(mcon 'conjoin-event '()) ;  There are no real words that really
(mcon 'conjoin-object '()) ; belong here.
(mcon 'conjure
      '((synonyms (arouse call-forth call-up-a-spirit change charm compel control dominate evoke influence invoke possess raise subdue summon sway))
	(tenses (conjures conjured conjuring))
       )
)
(mcon 'consent
      '((synonyms (accede acquiesce agree allow assent concede concur consent-to leave let permit subscribe suffer tolerate yield))
	(tenses (consents consented consenting))
       )
)
(mcon 'consider
      '((synonyms (analyze contemplate evaluate examine inquire-into investigate ponder reconsider reflect-on research scrutinize study survey think-about))
	(tenses (considers considered considering))
       )
)
(mcon 'constable
      '((superordinates (official lawman peacekeeper peace-officer))
	(synonyms (deputy marshal marshall sheriff))
	(plural (constables))
       )
)
(mcon 'contact
      '((superordinates (official))
	(subordinates (peacemaker arbiter moderator negotiator))
	(synonyms (mediator middleman go-between))
	(plural (contacts))
       )
)
(mcon 'contain
      '((synonyms (enclose include hold entail cover comprise encompass))
	(tenses (contains contained containing))
       )
)
(mcon 'continue
      '((synonyms (carry-on endure extend go go-on last persist press-on remain))
	(tenses (continues continued continuing))
       )
)
(mcon 'convene
      '((synonyms (meet gather))
	(tenses (convenes convened convening))
       )
)
(mcon 'converge-on
      '((synonyms (converge focus-on approach meet-at come-together))
       )
)
(mcon 'convince
      '((synonyms (persuade talk-into))
	(tenses (convinces convinced convincing))
       )
)
(mcon 'convinced
      '((synonyms (believing sure))
	(antonyms (hesitant uncertain))
       )
)
(mcon 'convince-that
      '((synonyms (convince persuade-that))
	(tenses (convinces-that convinced-that convincing-that))
       )
)
(mcon 'corrupt
      '((synonyms (reprobate crooked bribable dishonest 
		   dishonorable))
	(antonyms (good honest moral righteous straight virtuous))
       )
)
(mcon 'counselor
      '((superordinates (professional-person))
	(subordinates (judge justice magistrate))
	(synonyms (adviser advisor advocate attorney barrister counsel counselor-at-law lawyer solicitor))
	(plural (counselors))
       )
)
(mcon 'count
      '((superordinates (aristocrat nobleman))
	(synonyms (earl))
	(plural (counts))
       )
)
(mcon 'countess
      '((superordinates (aristocrat lady noblewoman peeress))
	(plural (countesses))
       )
)
(mcon 'country
      '((synonyms (land territory homeland))
	(plural (countries))
       )
)
(mcon 'country-of
      '((synonyms (country land-of homeland-of home-of))
	(plural (countries-of))
       )
)
(mcon 'courtier
      '((superordinates (adherent follower))
	(synonyms (apple-polisher bootlicker fawner flatterer flunky stooge sycophant toady truckler yes-man yes-man))
	(plural (courtiers))
       )
)
(mcon 'cousin
      '((superordinates (relation kin))
	(plural (cousins))
       )
)
(mcon 'coward
      '((superordinates (unfortunate-person normal-person))
	(synonyms (weakling sissy baby chicken))
	(plural (cowards))
       )
)
(mcon 'crawl
      '((synonyms (creep slide slither))
	(tenses (crawls crawled crawling))
       )
)
(mcon 'crawl-under
      '((synonyms (crawl creep-under slide-under slither-under))
	(tenses (crawls-under crawled-under crawling-under))
       )
)
(mcon 'creditor
      '((superordinates (professional-person))
	(synonyms (debtee))
	(antonyms (debtor borrower))
	(plural (creditors))
       )
)
(mcon 'critic
      '((superordinates (author composer writer))
	(synonyms (commentator reviewer heckler faultfinder nitpicker quibbler))
	(plural (critics))
       )
)
(mcon 'criticize
      '((synonyms (blame castigate censure condemn denounce reprehend reprove))
	(tenses (criticizes criticized criticizing))
       )
)
(mcon 'criticize-for
      '((synonyms (criticize blame-for censure-for condemn-for denounce-for reprehend-for reprove-for))
	(tenses (criticizes-for criticised-for criticizing-for))
       )
)
(mcon 'croak
      '((superordinates (make-noise))
	(synonyms (grumble squawk))
	(tenses (croaks croaked croaking))
       )
)
(mcon 'cross
      '((synonyms (traverse))
	(tenses (crosses crossed crossing))
       )
)
(mcon 'crossbow
      '((superordinates (bow weapon arm))
	(sub-parts (crossbow-bolts))
	(plural (crossbows))
       )
)
(mcon 'crossbow-bolt
      '((superordinates (missile projectile weapon))
	(part-of (crossbow quiver))
	(plural (crossbow-bolts))
       )
)
(mcon 'crow
      '((superordinates (carrion-bird bird))
	(plural (crows))
       )
)
(mcon 'crude
      '((synonyms (simple unpolished rough boorish obscene vulgar primitive coarse raw natural unfinished imperfect unrefined))
	(antonyms (fine delicate finished perfect refined processed complex))
       )
)
(mcon 'crumble
      '((synonyms (buckle collapse decay fall-apart))
	(tenses (crumbles crumbled crumbling))
       )
)
(mcon 'crumbling
      '((antonyms (solid firm secure))
       )
)
(mcon 'crush
      '((synonyms (break compact compress damage demolish mash press pulverize smash))
	(tenses (crushes crushed crushing))
       )
)
(mcon 'cub
      '((superordinates (child youngster))
	(plural (cubs))
       )
)
(mcon 'cudgel
      '((synonyms (batter beat club flog hit pummel thrash))
	(tenses (cudgels cudgeled cudgelled cudgeling cudgelling))
       )
)
(mcon 'cure
      '((synonyms (remedy curative))
	(plural (cures))
       )
)
(mcon 'curious
      '((synonyms (inquisitive prying))
	(antonyms (uninterested))
       )
)
(mcon 'curious-whether
      '((synonyms (curious inquisitive-whether))
	(antonyms (uninterested-whether))
       )
)
(mcon 'current
      '((synonyms (at-hand present recent))
	(antonyms (former old passe))
       )
)
(mcon 'curse
      '((synonyms (afflict anathematize blaspheme condemn damn denounce doom excommunicate execrate grieve harass hurt injure oppress persecute punish swear))
	(tenses (curses cursed cursing))
       )
)
(mcon 'curtain
      '((superordinates (screen cover concealment))
	(synonyms (drape drapery mantle pall veil))
	(plural (curtains))
       )
)
) ; end of semantics_c

(defun semantics_d ()

(mcon 'dance
      '((subordinates (tap-dance waltz shimmy reel))
	(tenses (dances danced dancing))
       )
)
(mcon 'danger
      '((synonyms (peril jeopardy risk threat))
	(plural (dangers))
       )
)
(mcon 'dangerous
      '((synonyms (hazardous perilous))
	(antonyms (harmless safe))
       )
)
(mcon 'daughter
      '((superordinates (girl relative kin child))
	(synonyms (descendant offspring))
	(plural (daughters))
       )
)
(mcon 'day
      '((superordinates (time))
	(sub-parts (dawn morning noon afternoon dusk evening night midnight))
	(plural (days))
       )
)
(mcon 'dead
      '((synonyms (obsolete lifeless deceased defunct extinct 
		   vanished expired departed inanimate))
	(antonyms (alive live living))
       )
)
(mcon 'death
      '((subordinates (suffocation smothering asphyxiation choking drowning starvation euthanasia))
	(synonyms (death dying demise))
	(antonyms (life living vitality))
	(plural (deaths))
       )
)
(mcon 'debt
      '((synonyms (indebtedness obligation liability dues bills charges deficit))
       )
)
(mcon 'debtor
      '((synonyms (borrower))
	(antonyms (creditor lender debtee))
	(plural (debtors))
       )
)
; As hard as it was to believe, I could not think of any one word antonyms for
; deceive.  The English language is filled with ways to describe trickery and
; falsehood, but has none to describe truth-telling and good deeds?
(mcon 'deceive
      '((synonyms (beguile betray con delude feign fool hoax lead-astray mislead seduce trick))
	(antonyms (tell-truth-to not-deceive))
	(tenses (deceives deceived deceiving))
       )
)
(mcon 'decide
      '((synonyms (choose conclude decree judge))
	(tenses (decides decided deciding))
       )
)
(mcon 'decree
      '((synonyms (dictate order prescribe ordain direct))
	(tenses (decrees decreed decreeing))
       )
)
(mcon 'decree-that
      '((synonyms (decree dictate-that order-that prescribe-that ordain-that))
	(tenses (decrees-that decreed-that decreeing-that))
       )
)
(mcon 'deer
      '((superordinates (ruminant mammal))
	(synonyms (buck doe elk fawn))
	(plural (deer))
       )
)
(mcon 'defeat
      '((synonyms (annihilate annul avoid baffle balk beat blast block break check circumvent conquer crush demolish destroy disrupt evade foil frustrate get-the-better-of have invalidate kill lick make-null make-void nullify outdo outmaneuver outwit over

come overpower overthrow overturn overwhelm put-down reduce refuse-to-approve repulse rout ruin sink smash smear smother stop subdue subjugate subvert surmount surpass swamp thrash throw thwart top topple trim trounce turn-back vacate vanquish veto wipe-o

ut wreck))
	(tenses (defeats defeated defeating))
       )
)
(mcon 'deformed
      '((synonyms (crooked defaced disfigured grotesque malformed marred miscreate miscreated misshapen monstrous scarred twisted))
	(antonyms (beautiful healthy sound unharmed))
       )
)
(mcon 'delude
      '((synonyms (beguile deceive dupe fool mislead trick))
	(tenses (deludes deluded deluding))
       )
)
(mcon 'demagogue
      '((superordinates (bigot partisan radical zealot))
	(synonyms (agitator rabble-rouser trouble-maker))
	(plural (demagogues))
       )
)
(mcon 'demand
      '((synonyms (ask-for call-for importune-for insist-upon request solicit))
	(tenses (demands demanded demanding))
       )
)
(mcon 'denounce
      '((synonyms (accuse anathematize arraign ascribe attack blame castigate censure charge condemn criminate criticize curse damn decry deplore disapprove-of excoriate execrate hold-accountable-for impeach incriminate indight name rebuke reprehend repri

mand reprobate reprove tax))
	(tenses (denounces denounced denouncing))
       )
)
(mcon 'depart
      '((synonyms (leave quit retire withdraw))
	(antonyms (remain rest stay))
	(tenses (departs departed departing))
       )
)
(mcon 'depose
      '((synonyms (dethrone topple overthrow affirm asseverate attest declare-under-oath degrade demote depone discharge displace downgrade reduce remove-from-office strip-of-rank swear testify warrant witness))
	(tenses (deposes deposed deposing))
       )
)
(mcon 'deputy
      '((superordinates (official))
	(synonyms (adjutant aide aide-de-camp attache lieutenant subordinate constable marshal marshall sheriff))
	(plural (deputies))
       )
)
(mcon 'desert
      '((synonyms (abandon barren betray cast-off defect desolate disappoint ditch fail forsake inhospitable leave leave-behind let-down prove-faithless-to quit reject revolt strand turn waste withdraw-from))
	(tenses (deserts deserted deserting))
       )
)
(mcon 'desire
      '((synonyms (wish hope want))
	(tenses (desires desired desiring))
       )
)
(mcon 'destiny
      '((synonyms (fate chance fortune luck happenstance destination inevitability))
	(plural (destinies))
       )
)
(mcon 'destroy
      '((superordinates (damage))
	(synonyms (ruin devastate ravage obliterate wreck demolish
		   annihilate eradicate))
	(tenses (destroys destroyed destroying))
       )
)
(mcon 'destruction
      '((synonyms (ruin ruination perdition damnation devastation ravage havoc desolation dissolution disintegration))
	(antonyms (construction reconstruction restoration rehabilitation rebuilding))
	(plural (destructions))
       )
)
(mcon 'devour
      '((synonyms (consume eat absorb destroy ravage swallow))
	(tenses (devours devoured devouring))
       )
)
(mcon 'dictator
      '((superordinates (official ruler))
	(synonyms (ruler tyrant despot autocrat oligarch))
	(plural (dictators))
       )
)
(mcon 'die
      '((synonyms (cease stop perish end expire cease))
	(tenses (dies died dying))
       )
)
(mcon 'difficult
      '((synonyms (arduous baffling demanding onerous troublesome))
	(antonyms (easy possible simple))
       )
)
(mcon 'dig-for
      '((synonyms (burrow-for mine-for excavate-for dig probe-for))
       )
)
(mcon 'dinner
      '((superordinates (meal repast))
	(plural (dinners))
       )
)
(mcon 'disarm
      '((synonyms (render-powerless paralyze))
	(tenses (disarms disarmed disarming))
       )
)
(mcon 'disavow
      '((synonyms (deny disclaim recall recant renounce repudiate retract swallow take-back unsay withdraw))
	(tenses (disavows disavowed disavowing))
       )
)
(mcon 'discover
      '((synonyms (ascertain come-across come-upon detect find find-out happen-upon notice perceive see spot stumble-on stumble-upon turn-up uncover))
	(tenses (discovers discovered discoverring))
       )
)
(mcon 'discredit
      '((synonyms (abase asperse bring-reproach calumniate cast-doubt defame demolish destroy disgrace dishonor humiliate invalidate malign overturn reflect ruin shame slander speak-ill-of traduce vilify))
	(tenses (discredits discredited discrediting))
       )
)
(mcon 'disguise
      '((synonyms (cloak mask camouflage conceal hide screen veil))
	(antonyms (reveal uncloak unveil))
	(tenses (disguises disguised disguising))
       )
)
(mcon 'dislike
      '((synonyms (detest hate mind object-to))
	(antonyms (like love accept admire fancy enjoy approve-of))
       )
)
(mcon 'dispute
      '((synonyms (debate contest argue))
	(tenses (disputes disputed disputing))
       )
)
(mcon 'disregard
      '((synonyms (defy disobey dispense-with do-without forget ignore neglect omit overlook transgress violate))
	(tenses (disregards disregarded disregarding))
       )
)
(mcon 'dissatisfied
      '((synonyms (discontent displeased malcontent unhappy))
	(antonyms (content contented happy pleased))
       )
)
(mcon 'dissatisfied-with
      '((synonyms (dissatisfied discontent-with displeased-with unhappy-with))
	(antonyms (content-with happy-with))
       )
)
(mcon 'dissuade
      '((synonyms (deflect deter discourage distract divert drive-away hinder repel sway))
	(tenses (dissuades dissuaded dissuading))
       )
)
(mcon 'distinguish
      '((synonyms (classify demarcate differentiate discern discriminate identify set-apart))
	(tenses (distinguishes distinguished distinguishing))
       )
)
; To distress makes someone worried, therefore this should be in a rule.
(mcon 'distress
      '((synonyms (trouble plague irk burden vex worry bother
		   concern disturb torment agitate))
	(tenses (distresses distressed distressing))
       )
)
(mcon 'district
      '((synonyms (community neighborhood sector area region))
	(plural (districts))
       )
)
(mcon 'divide
      '((synonyms (partition distribute share apportion))
	(tenses (divides divided dividing))
       )
)
(mcon 'divine
      '((synonyms (blessed godly holy inspired religious sacred sublime))
	(antonyms (earthly profane unspiritual))
       )
)
(mcon 'do
      '((synonyms (act effect perform perpetrate work))
	(tenses (does did doing))
       )
)
(mcon 'doctor
      '((superordinates (public-servant))
	(synonyms (physician medical-practitioner))
	(plural (doctors))
       )
)
(mcon 'dog
      '((superordinates (canid mammal))
	(subordinates (basset basset-hound beagle bird-dog bitch boxer bulldog chihuahua collie dachshund dalmatian doberman doberman-pinscher foxhound german-shepherd golden-retriever great-dane great-pyrenees greyhound hound hunting-dog husky irish-wolfhound l

hasa-apso mongrel mutt newfoundland pekinese pekingese pointer pooch poodle pug pup puppy retriever russian-wolfhound saint-bernard samoyed schnauzer setter sheepdog spaniel terrier toy-poodle watchdog wild-dog wolfhound))
	(synonyms (canine))
	(plural (dogs))
       )
)
(mcon 'dolphin
      '((superordinates (whale))
	(subordinates (bottlenosed-dolphin))
	(plural (dolphins))
       )
)
(mcon 'donate-to
      '((synonyms (donate contribute-to give-to))
	(tenses (donates-to donated-to donating-to))
       )
)
(mcon 'donkey
      '((superordinates (ass jackass))
	(synonyms (burro))
	(plural (donkeys))
       )
)
(mcon 'dosage
      '((synonyms (amount potency portion))
       )
)
(mcon 'dove
      '((superordinates (bird fowl pigeon))
	(subordinates (mourning-dove ringdove turtledove))
	(synonyms (pigeon squab))
	(plural (doves))
       )
)
(mcon 'dream
      '((synonyms (have-a-vision aspire conceive conceive-of desire fancy fantasize imagine long suppose wish-for yearn))
	(tenses (dreams dreamed dreaming))
       )
)
(mcon 'dream-of
      '((synonyms (vision-of dream vision fantasy fantasy-about))
	(plural (dreams-of))
       )
)
(mcon 'drink
      '((synonyms (digest eat imbibe partake-of sip swallow swig take))
	(tenses (drinks drank drinking))
       )
)
(mcon 'drink-from
      '((synonyms (drink imbibe-from sip-from swig-from))
	(tenses (drinks-from drank-from drinking-from))
       )
)
(mcon 'drop
      '((synonyms (fall let_fall plunge plummet dive release))
	(antonyms (retain))
	(variations (drop-in))
	(tenses (drops dropped dropping))
       )
)
(mcon 'drop-in
      '((synonyms (drop throw-in toss-in))
	(tenses (drops-in dropped-in dropping-in))
       )
)
(mcon 'drown
      '((synonyms (sink submerge deluge))
	(tenses (drowns drowned drowning))
       )
)
(mcon 'drowned
      '((synonyms (dead sunk foundered suffocated asphyxiated))
       )
)
(mcon 'dry-up
      '((synonyms (dry shrivel shrivel-up wither wizen wrinkle))
	(tenses (dries-up dried-up drying-up))
       )
)
(mcon 'duel
      '((synonyms (fight combat battle))
	(plural (duels))
       )
)
(mcon 'duke
      '((superordinates (aristocrat nobleman))
	(plural (dukes))
       )
)
(mcon 'dumb
      '((synonyms (dim-witted half-witted obtuse simple-minded stupid thick thickheaded))
	(antonyms (clever intelligent))
       )
)
(mcon 'dying
      '((synonyms (expiring moribund))
	(antonyms (alive))
       )
)
) ; end of semantics_d

