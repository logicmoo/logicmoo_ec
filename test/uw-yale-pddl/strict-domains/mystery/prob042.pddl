(DEFINE (PROBLEM STRIPS-MYSTY-1)
   (:DOMAIN MYSTERY-STRIPS)
   (:OBJECTS OKRA MUFFIN TOMATO FLOUNDER CHICKEN TRIUMPH SATIETY
             ABRASION SCIATICA GRIEF LONELINESS DEPRESSION JEALOUSY DREAD
             PROSTATITIS HANGOVER ANGINA GOIAS MARS EARTH NEPTUNE)
   (:INIT (FOOD OKRA)
          (FOOD MUFFIN)
          (FOOD TOMATO)
          (FOOD FLOUNDER)
          (FOOD CHICKEN)
          (PLEASURE TRIUMPH)
          (PLEASURE SATIETY)
          (PAIN ABRASION)
          (PAIN SCIATICA)
          (PAIN GRIEF)
          (PAIN LONELINESS)
          (PAIN DEPRESSION)
          (PAIN JEALOUSY)
          (PAIN DREAD)
          (PAIN PROSTATITIS)
          (PAIN HANGOVER)
          (PAIN ANGINA)
          (PROVINCE GOIAS)
          (PLANET MARS)
          (PLANET EARTH)
          (PLANET NEPTUNE)
          (LOCALE CHICKEN GOIAS)
          (CRAVES PROSTATITIS TOMATO)
          (EATS TOMATO OKRA)
          (EATS OKRA MUFFIN)
          (EATS MUFFIN CHICKEN)
          (LOCALE FLOUNDER GOIAS)
          (LOCALE MUFFIN GOIAS)
          (EATS TOMATO FLOUNDER)
          (EATS OKRA TOMATO)
          (CRAVES SATIETY FLOUNDER)
          (CRAVES ABRASION OKRA)
          (EATS CHICKEN TOMATO)
          (CRAVES JEALOUSY TOMATO)
          (ORBITS MARS EARTH)
          (CRAVES LONELINESS MUFFIN)
          (CRAVES DREAD TOMATO)
          (EATS CHICKEN FLOUNDER)
          (EATS FLOUNDER TOMATO)
          (ORBITS EARTH NEPTUNE)
          (CRAVES ANGINA CHICKEN)
          (CRAVES HANGOVER FLOUNDER)
          (CRAVES GRIEF MUFFIN)
          (HARMONY TRIUMPH NEPTUNE)
          (HARMONY SATIETY EARTH)
          (CRAVES DEPRESSION MUFFIN)
          (EATS TOMATO CHICKEN)
          (CRAVES TRIUMPH MUFFIN)
          (EATS FLOUNDER MUFFIN)
          (EATS FLOUNDER CHICKEN)
          (CRAVES SCIATICA OKRA)
          (EATS MUFFIN OKRA)
          (EATS MUFFIN FLOUNDER)
          (LOCALE OKRA GOIAS)
          (LOCALE TOMATO GOIAS)
          (EATS CHICKEN MUFFIN))
   (:GOAL (AND (CRAVES PROSTATITIS FLOUNDER))))