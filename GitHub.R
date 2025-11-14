#install.packages("usethis")
library(usethis)

usethis::use_git_config(
  user.name = "NicoleDrewitz",
  user.email = "nkj1@unak.is"
)

usethis::create_github_token()

######################### test code
ggplot2::theme_set(ggplot2::theme_minimal())
library(palmerpenguins)
library(ggplot2)

mass_flipper <- ggplot(data = penguins,
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

mass_flipper

# =============================================================================
# PCA https://allisonhorst.github.io/palmerpenguins/articles/pca.html
# =============================================================================
# PCA
library(palmerpenguins)
library(corrr)
library(GGally)
library(recipes)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_minimal())

# corplot
library(corrr)
penguins_corr <- penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  correlate() %>%
  rearrange()
penguins_corr

# pairwise plot matrix (best plot combo!!!)
penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species),
                  columns = c("flipper_length_mm", "body_mass_g",
                              "bill_length_mm", "bill_depth_mm")) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# PCA: value results table
library(recipes)
penguin_recipe <-
  recipe(~., data = penguins) %>%
  update_role(species, island, sex, year, new_role = "id") %>%
  step_naomit(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

penguin_pca <-
  penguin_recipe %>%
  tidy(id = "pca")

penguin_pca

#tidy table
penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  tidyr::drop_na() %>%
  scale() %>%
  prcomp() %>%
  .$rotation

# variance: bar graph
penguin_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance") %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "#b6dfe2") +
  xlim(c(0, 5)) +
  ylab("% of total variance")

# PCA plots
library(ggplot2)
penguin_pca %>%
  mutate(terms = tidytext::reorder_within(terms,
                                          abs(value),
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )
# =============================================================================
# ASCII Art
# https://dahtah.github.io/imager/ascii_art.html
# =============================================================================

##The tidyverse package loads dplyr, purrr, etc.
library(tidyverse)
library(imager)
##Optional: cowplot has nicer defaults for ggplot
library(cowplot)

# LOAD IMAGE
# =============================================================================
im <- load.image("https://upload.wikimedia.org/wikipedia/commons/9/98/Wolverine_03.jpg")
plot(im)

# GENERATE ACS CHARACTERS
# =============================================================================
asc <- gtools::chr(38:126) #We use a subset of ASCII, R doesn't render the rest
head(asc,10)

#Draw some text on a white background:
txt <- imfill(50,50,val=1) %>% implot(text(20,20,"Blah"))
txt
# test plot
plot(txt,interp=FALSE)

#A function that plots a single character and measures its lightness
g.chr <- function(chr) implot(imfill(50,50,val=1),text(25,25,chr,cex=5)) %>% grayscale %>% mean
g <- map_dbl(asc,g.chr)
n <- length(g)
plot(1:n,sort(g),type="n",xlab="Order",ylab="Lightness")
text(1:n,sort(g),asc[order(g)])

# DRAFT RENDER
# =============================================================================
#Sort the characters by increasing lightness
char <- asc[order(g)]
#Convert image to grayscale, resize, convert to data.frame
d <- grayscale(im) %>% imresize(.1)  %>% as.data.frame
#Quantise
d <- mutate(d,qv=cut_number(value,n) %>% as.integer)
#Assign a character to each quantised level
d <- mutate(d,char=char[qv])
#Plot
ggplot(d,aes(x,y))+geom_text(aes(label=char),size=1)+scale_y_reverse()

# MATCH IMAGE PATCHES TO CHARACTERS
# =============================================================================
#psize is the patch size in pixels (we'll use odd patch sizes for simplicity)
psize <- 5
#center of the patch
cen <- ceiling(psize/2)

#produces a grid of coordinates for the center of each patch
gr <- grayscale(im) %>% pixel.grid %>%
  filter((x %% psize)==cen,(y %% psize) == cen)

plot(im,xlim=c(200,250),ylim=c(200,250))
with(gr,points(x,y,cex=1,col="red"))

#extract an image patch at each point on the grid
ptch <- extract_patches(im,gr$x,gr$y,psize,psize)

#NB: ptch is an image list, in which each element is an image patch
ptch

# RENDER IMAGE PATCHES
# =============================================================================
syms <- 1:25

render <- function(sym) implot(imfill(psize,psize,val=1),points(cen,cen,pch=sym,cex=1)) %>% grayscale
ptch.c <- map(syms,render)
plot(ptch.c[[4]],interp=FALSE)

#Convert patches to a long matrix
Pim <- map(ptch,as.vector) %>% do.call(rbind,.)
Pc <- map(ptch.c,as.vector) %>% do.call(rbind,.)
nn <- nabor::knn(Pc,Pim,1)$nn.idx
mutate(gr,sym=syms[nn]) %$% plot(x,y,pch=sym,cex=.2,ylim=c(height(im),1))
