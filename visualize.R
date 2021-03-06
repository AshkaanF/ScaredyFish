## ---
## This contains functions that do various bits
## of the agent based model
## ---

## prep
if( !require( RColorBrewer ) ) { install.packages( 'RColorBrewer' ) }; library( RColorBrewer )
if( !require( magrittr ) ) { install.packages( 'magrittr' ) }; library( magrittr )
if( !require( ggnetwork ) ) { install.packages( 'ggnetwork' ) }; library( ggnetwork )
if( !require( ggplot2 ) ) { install.packages( 'ggplot2' ) }; library( ggplot2 )
if( !require( dplyr ) ) { install.packages( 'dplyr' ) }; library( dplyr )
if( !require( OpenImageR ) ) { install.packages( 'OpenImageR' ) }
if( !require( reshape2 ) ) { install.packages( 'reshape2' ) }


## ---
## Initialize
## ---

## load data
res <- read.table( './out.tsv', sep = '\t' )

## add header
names( res ) <- c( 'time', 'id', 'x', 'y', 'state', 'in.a.patch' )

## remove goners
res <- res[ res$x > -1e5, ]

## get range of my arena
arena_x <- range( res$x ) + c(-1, 1)
arena_y <- range( res$y ) + c(-1, 1)

## aspect ratio of plot
my.aspect <- diff( arena_x ) / diff( arena_y )

## load and melt
tile <- as.matrix( read.table( './reef.tsv', header = T, sep = '\t' ) )
# tile <- OpenImageR::down_sample_image( tile, factor = 1, gaussian_blur = T )
tile.grid <- melt( tile )
tile.grid$Var2 <- as.numeric( gsub( 'V', '', tile.grid$Var2 ) )
# tile.grid$value <- ifelse( tile.grid$value == 'true', 1, 0 )

## plot
ggplot(data = tile.grid, aes( Var1, Var2, fill = ( value ) ) ) +
  geom_raster() +
  scale_fill_gradientn( colours = c( '#000000', '#f7f7f7' ), guide = F ) +
  theme( aspect.ratio = my.aspect ) +
  coord_flip()



## -
## work
## -
for( f in 2 : max( res$time ) ) {
  
  ## grab a dataframe you want to use
  current <- tile.grid
  
  ## append column
  current$ratio <- current$value
  
  ## grab stats for each tile
  current.res <- res[ res$time == f, ]
  
  ## summarise tiles
  sum.res <- current.res %>%
    group_by( 'rx' = round( x ), 'ry' = round( y ) ) %>%
    summarise( 'n' = length( state ),
               'r' = sum( state ) ) %>%
    as.data.frame()
  
  ## fill data frame
  current$ratio[ !is.na( prodlim::row.match( current[, 1 : 2 ], sum.res[, 1 : 2 ] ) ) ] <- 
  current$ratio[ !is.na( prodlim::row.match( current[, 1 : 2 ], sum.res[, 1 : 2 ] ) ) ] + 
    ( sum.res$r[ prodlim::row.match( current[, 1 : 2 ], sum.res[, 1 : 2 ] ) ][ !is.na( prodlim::row.match( current[, 1 : 2 ], sum.res[, 1 : 2 ] ) ) ] /
        sum.res$n[ prodlim::row.match( current[, 1 : 2 ], sum.res[, 1 : 2 ] ) ][ !is.na( prodlim::row.match( current[, 1 : 2 ], sum.res[, 1 : 2 ] ) ) ] )
  
  ## if
  # if( f == 2 ) { last <- current }
  
  ## average from last step
  # current$ratio <- ( current$ratio + ( current$ratio + last$ratio ) / 2 ) / 2
  
  ##
  ## can i show when groups initiate? 
  ##
  
  ## ggplot
  gg.tile <- ggplot(data = current, aes( x = Var1, y = Var2, fill = ratio ) ) +
    theme_blank() +
    geom_raster() +
    geom_point( data = current[ current$ratio > 1, ],
                aes( x = Var1, y = Var2, fill = ratio, size = ratio ),
                shape = 21, alpha = 0.75, stroke = 0.1, colour = '#f7f7f7',
                inherit.aes = F ) +
    scale_fill_gradientn( colours = c( '#000000', '#d9d9d9', '#f0f0f0', '#ffff33', '#e41a1c' ), 
                          limits = c( 0, 2 ), guide = F ) +
    scale_size_continuous( range = c( 0.03, 0.9 ), limits = c( 1, 2 ), guide = F ) +
    theme( aspect.ratio = my.aspect,
           panel.background = element_rect(fill = "black", color  =  NA),
           plot.background = element_rect(color = "black", fill = "black" ) ) +
    coord_flip()
  
  
  ##
  ## viz output
  ##
  
  ## generate a filename for the frame
  fn <- paste('../animation_assets/z_frame', 
              sprintf("%05d", f), 
              sep = '_') %>%
    paste(., 'png', sep = '.')
  
  ## vector
  png(filename = fn,
      width     = 2.95,
      height    = 2.95,
      units     = 'in',
      res       = 480,
      bg = 'black'
  )
  
  ## print network
  print( gg.tile )
  
  ## print
  dev.off()
  
  ## drop a line
  cat( f, 'printed\n' )
  
  ## save last frame
  # last = current
  
}









## -
## work
## -
for( f in 2 : max( res$time ) ) {
  
  ## grab a dataframe you want to use
  current <- res[res$time <= f & res$time > (f - 10), ]
  
  ## calculate transparency based on time
  current$transp <- exp( current$time / max( current$time ) )
  
  ## index for the current time
  current$curr <- rep( 0 )
  current$curr[current$time == max(current$time)] <- 1
  
  ##
  ## can i show when groups initiate? 
  ##
  
  ## order factors in color mapping
  myColors <- c( '#1d91c0', '#e41a1c', '#fdae61' )
  names(myColors) <- c('0', '1', 'Cool')
  colScale <- scale_colour_manual(values = myColors, guide = F)
  
  ## ggnetwork plot
  gg.net <- ggplot(data = current, aes(x = x, y = y, group = id, 
                                       colour = state, 
                                       alpha = transp ) ) +
    geom_raster(data = tile.grid,
              aes( Var1, Var2, fill = as.numeric( value ) ),
              alpha = 0.5,
              inherit.aes = F ) +
    # geom_edges(color = '#ffffff',
    #            size = 0.2,
    #            alpha = 0.1,
    #            arrow = arrow( length = unit( 0, 'pt' ), type = 'closed' ) ) + # draw edge layer
    geom_path(size = 0.04, colour = '#d9d9d9' ) +
    geom_point( data = current[ current$curr == 1, ], aes( colour = as.factor( state ), size = as.factor( state ) ), shape = 20, alpha = 0.55 ) + # draw node layer
    theme_blank() +
    theme(legend.position = "bottom") +
    scale_x_continuous(limits = arena_x) +
    scale_y_continuous(limits = arena_y) +
    scale_fill_gradientn( colours = c( '#000000', '#737373' ), guide = F ) +
    colScale +
    scale_size_manual(values = c( '1' = 0.05, '0' = 0.01, 'Cool' = 0.025 ), guide = F ) +
    scale_alpha_continuous(range = c( 0.1, 1 ), trans = 'sqrt', guide = F ) +
    theme( aspect.ratio = my.aspect,
           panel.background = element_rect(fill = "black", color  =  NA),
           plot.background = element_rect(color = "black", fill = "black") ) +
    coord_flip()
  
  
  ##
  ## viz output
  ##
  
  ## generate a filename for the frame
  fn <- paste('../animation_assets/z_frame', 
              sprintf("%05d", f), 
              sep = '_') %>%
    paste(., 'png', sep = '.')
  
  ## vector
  png(filename = fn,
      width     = 2.95,
      height    = 2.95,
      units     = 'in',
      res       = 480,
      bg = 'black'
  )
  
  ## print network
  print(gg.net)
  
  ## print
  dev.off()
  
  ## drop a line
  cat(f, 'printed\n')
  
}







## ----------------------
## big results
## ----------------------
library( dplyr )
library( magrittr )
library( tibble )
library(data.table)

## data
res <- fread( './results.tsv.gz', sep = '\t', header = F )

## add header
names( res ) <- c( 'time', 'id', 'x', 'y', 'state', 'in.a.patch', 'sim', 'inhibition' )

## remove goners
res <- res[ res$x > -1e5, ]

## summarize
sum.res <- res[ res$state == 1, ] %>%
  dplyr::group_by( sim, inhibition ) %>%
  dplyr::summarise( 'cascade.length' = length( unique( id ) ) ) %>%
  as.data.frame() 

## make fractions
sum.res$frac <- sum.res$cascade.length / 4000

## boxplot
ggplot( sum.res, aes( x = as.factor( inhibition ), y = cascade.length, fill = inhibition ) ) +
  theme_classic() +
  scale_y_log10() +
  ylab( 'Cascade size (number of agents)' ) +
  xlab( 'Inhibition strength' ) +
  geom_boxplot( fatten = 0, width = 0.55, size = 0.65, coef = 1e3, fill = '#ffffff' ) +
  stat_summary( geom = 'point', fun = 'mean', shape = 21, size = 1.5, stroke = 1, alpha = 0.75 ) +
  scale_fill_gradientn( colours = viridis::inferno( 11 ),
                        guide = guide_colorbar(label = TRUE,
                                               draw.ulim = TRUE,
                                               draw.llim = TRUE,
                                               frame.colour = 'black',
                                               frame.linewidth = 1,
                                               ticks = TRUE,
                                               nbin = 6,
                                               title.position = 'left',
                                               title.theme = element_text(angle = 90, size = 8),
                                               label.position = 'right',
                                               barwidth = 0.4,
                                               barheight = 7,
                                               direction = 'vertical') )

## density
ggplot( sum.res, aes( x = frac, y = ..count.. / sum( ..count.. ), fill = inhibition, group = inhibition ) ) +
  theme_classic() +
  ylab( 'Proportion' ) +
  xlab( 'Cascade size (fraction of agents)' ) +
  scale_y_log10() +
  stat_bin( bins = 50, colour = '#000000', geom = 'point', shape = 21, position = 'identity', size = 2 ) +
  scale_fill_gradientn( colours = viridis::magma( 4 ),
                        guide = guide_colorbar(label = TRUE,
                                               draw.ulim = TRUE,
                                               draw.llim = TRUE,
                                               frame.colour = 'black',
                                               frame.linewidth = 1,
                                               ticks = TRUE,
                                               nbin = 6,
                                               title.position = 'left',
                                               title.theme = element_text(angle = 90, size = 8),
                                               label.position = 'right',
                                               barwidth = 0.4,
                                               barheight = 7,
                                               direction = 'vertical') )

## joy plot
library(ggridges)
ggplot( sum.res, aes( x = cascade.length,
                  height = ..ndensity..,
                  group = inhibition, fill = inhibition ) ) +
  geom_density_ridges2( aes( y = as.factor( inhibition ) ),
                        stat = 'binline',
                        scale = 1.5, rel_min_height = 0.01,
                        binwidth = 0.22,
                        alpha = 0.5,
                        size = 0.5 ) +
  xlab( 'Cascade size' ) +
  ylab( 'Inhibition strength' ) +
  # scale_x_continuous( expand = c(0, 0) ) +
  scale_x_log10( expand = c(0, 0) ) +
  scale_fill_gradientn(
    colours = viridis::inferno( 15 ),
    limits = c(0, 1.2),
    guide = F ) +
  theme_ridges( font_size = 13, grid = TRUE ) +
  theme( axis.title = element_text( size = 10 ),
         axis.text = element_text( size = 8 ) )

