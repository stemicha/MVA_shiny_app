hcpc.plot<-function (x, axes = c(1, 2), choice = "3D.map", rect = TRUE, 
          draw.tree = TRUE, ind.names = TRUE, t.level = "all", title = NULL, 
          new.plot = FALSE, max.plot = 15, tree.barplot = TRUE, centers.plot = FALSE,clust.col=NULL,names.cex=1,pos.lab=1,points.cex=10,legend=TRUE, 
          ...) 
{
  f.draw.tree = function(X, merge, height, dimens, t.level, 
                         ind.names, axes, xlim = NULL, vec = NULL, scale.y = NULL, 
                         xlab = NULL, ylab = NULL, y.ticklabs = NULL, ...) {
    ax1 = axes[1]
    ax2 = axes[2]
    names.ind = row.names(X)
    X$clust = as.numeric(X$clust)
    nb.clust = max(X$clust)
    if (class(t.level) == "character") {
      if (t.level == "centers") 
        t.level = nb.clust
      if (t.level == "all") 
        t.level = nrow(merge) + 1
    }
    leg = NULL
    if (dimens == 2) {
      main = NULL
    }
    if (dimens == 3) {
      if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
        dev.new()
      if (vec) {
        X[, ax1] = res$data.clust[, 1]
        x = X[, ax1]
        X[, ax2] = seq(min(x)/1000, max(x)/1000, length.out = length(x))
      }
      else {
        if (ncol(X) == 2) {
          X = X[, c(1, 1, 2)]
          x = X[, 1]
          X[, 2] = seq(min(x)/1000, max(x)/1000, length.out = length(x))
        }
        else x = X[, ax1]
      }
      y = X[, ax2]
      z = rep(0, nrow(X))
      X$clust = as.factor(as.numeric(X$clust))
      cluster.color<-X$clust
      cluster.color<-unlist(sapply(cluster.color,function(x) clust.col[as.numeric(x)]))
      levs = levels(X$clust)
      cluster.color.levs<-levs
      cluster.color.levs<-unlist(sapply(cluster.color.levs,function(x) clust.col[as.numeric(x)]))
      
      if (!vec) {
        if (is.null(xlab)) 
          xlab = paste("Dim ", ax1, " (", round(res$call$t$res$eig[ax1, 
                                                                   2], digits = 2), "%)", sep = "")
        if (is.null(ylab)) 
          ylab = paste("Dim ", ax2, " (", round(res$call$t$res$eig[ax2, 
                                                                   2], digits = 2), "%)", sep = "")
      }
      else {
        y.ticklabs = rep(" ", 2)
        ylab = " "
        xlab = " "
      }
      scale.y = ((max(y) - min(y))/(max(x) - min(x)))
      s = scatterplot3d::scatterplot3d(x, y, z, zlim = c(0, 
                                                         max(height)), xlab = xlab, ylab = ylab, zlab = "height", cex.axis=1, 
                                       box = FALSE, color = if(length(clust.col)==0){X$clust}else{cluster.color}, pch = 20, scale.y = scale.y, 
                                       y.ticklabs = y.ticklabs, ...)
      for (i in 1:nb.clust) leg = c(leg, paste("cluster", 
                                               levs[i], " ", sep = " "))
      if(legend==T){
        legend("topleft", leg, text.col = if(length(clust.col)==0){as.numeric(levels(X$clust))}else{cluster.color.levs}, 
             cex = 0.8)}
      if (ind.names) {
        for (i in 1:nrow(X)) text(s$xyz.convert(x[i], 
                                                y[i], 0), cex = names.cex, names.ind[i], col = as.vector(if(length(clust.col)==0){X$clust}else{cluster.color})[i], 
                                  pos = pos.lab)
      }
    }
    aa = matrix(ncol = 4, nrow = nrow(merge))
    for (i in 1:nrow(merge)) {
      if (merge[i, 1] < 0) {
        x1 = X[-merge[i, 1], ax1]
        y1 = X[-merge[i, 1], ax2]
        h1 = 0
        w1 = 1
      }
      else {
        x1 = aa[merge[i, 1], 1]
        y1 = aa[merge[i, 1], 2]
        w1 = aa[merge[i, 1], 4]
        h1 = aa[merge[i, 1], 3]
      }
      if (merge[i, 2] < 0) {
        x2 = X[-merge[i, 2], ax1]
        y2 = X[-merge[i, 2], ax2]
        h2 = 0
        w2 = 1
      }
      else {
        x2 = aa[merge[i, 2], 1]
        y2 = aa[merge[i, 2], 2]
        w2 = aa[merge[i, 2], 4]
        h2 = aa[merge[i, 2], 3]
      }
      aa[i, 1] = (w1 * x1 + w2 * x2)/(w1 + w2)
      aa[i, 2] = (w1 * y1 + w2 * y2)/(w1 + w2)
      if (i <= nrow(merge) - t.level + 1) 
        height[i] = 0
      aa[i, 3] = height[i]
      aa[i, 4] = w1 + w2
      if (i > (nrow(merge) - t.level + 1)) {
        if (dimens == 3) 
          s$points3d(rbind(c(x1, y1, h1), c(x1, y1, height[i]), 
                           c(x2, y2, height[i]), c(x2, y2, h2)), lty = 1, 
                     type = "o", pch = "")
        if (dimens == 2) 
          lines(c(x1, x2), c(y1, y2), lwd = (height[i]/max(height)) * 
                  3)
      }
    }
    if (dimens == 3) {
      list.centers = by(X[, -ncol(X), drop = FALSE], X$clust, 
                        colMeans)
      centers = matrix(unlist(list.centers), ncol = ncol(X) - 
                         1, byrow = TRUE)
      for (l in 1:nb.clust) {
        if (centers.plot) {
          s$points3d(centers[l, ax1], centers[l, ax2], 
                     0, col = levs[l], type = "o", pch = ".", 
                     cex = points.cex)
          text(s$xyz.convert(centers[l, ax1], centers[l, 
                                                      ax2], 0), cex = names.cex, leg[l], col = cluster.color.levs[l], 
               pos = 1)
        }
      }
    }
  }
  res = x
  X = res$call$X
  max = res$call$max
  min = res$call$min
  max.plot = max(res$call$max, 15)
  nb.clust = length(levels(X$clust))
  levs = levels(X$clust)
  if (choice == "tree") {
    if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
      dev.new()
    if (is.null(title)) 
      title = "Hierarchical clustering"
    if (tree.barplot) {
      def.par <- par(no.readonly = TRUE)
      par(mar = c(0.5, 2, 0.75, 0))
      lay = matrix(ncol = 5, nrow = 5, c(2, 4, 4, 4, 4, 
                                         2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 
                                         1, 3, 3, 3, 3))
      layout(lay, respect = TRUE)
      vec = res$call$t$inert.gain[1:max.plot]
      barplot(height = vec, col = c(rep("black", nb.clust - 
                                          1), rep("grey", max(max, 15) - nb.clust + 1)), 
              space = 0.9)
      plot(x = 1, xlab = "", ylab = "", main = "", col = "white", 
           axes = FALSE)
      text(1, 1, title, cex = 2)
      plot(x = 1, xlab = "", ylab = "", main = "", col = "white", 
           axes = FALSE)
      legend("top", "inertia gain  ", box.lty = NULL, cex = 1)
    }
    plot(res$call$t$tree, hang = -1, xlab = "", sub = "", 
         ...)
    if (rect) {
      y = (res$call$t$tree$height[length(res$call$t$tree$height) - 
                                    nb.clust + 2] + res$call$t$tree$height[length(res$call$t$tree$height) - 
                                                                             nb.clust + 1])/2
      ordColo <- unique(res$call$X$clust[res$call$t$tree$order])
      if(length(clust.col)==0){ordColo <- unique(res$call$X$clust[res$call$t$tree$order])}else{ordColo <-unlist(sapply(unique(res$call$X$clust[res$call$t$tree$order]),function(x) clust.col[as.numeric(x)]))}
      rect = rect.hclust(res$call$t$tree, h = y, border = ordColo)
    }
  }
  if (choice == "3D.map") {
    if (is.null(title)) 
      title = "Hierarchical clustering on the factor map"
    f.draw.tree(t.level = t.level, X, merge = res$call$t$tree$merge, 
                height = res$call$t$tree$height, dimens = 3, ind.names = ind.names, 
                axes = axes, vec = res$call$vec, ...)
    title(title)
  }
  if (choice == "bar") {
    if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
      dev.new()
    vec = res$call$t$inert.gain[1:max.plot]
    names.arg = NULL
    if (is.null(title)) 
      title = "Inter-cluster inertia gains"
    for (i in 1:(max.plot)) names.arg = c(names.arg, paste(i, 
                                                           "-", i + 1, sep = ""))
    barplot(vec, names.arg = names.arg, col = c(rep(1, nb.clust - 
                                                      1), rep(18, length(vec) - nb.clust + 1)), main = title, 
            xlab = "level of cutting", ylab = "Inertia")
  }
  if (choice == "map") {
    if (!res$call$vec) {
      if (is.null(title)) 
        title = "Factor map"
      Y = X[, -ncol(X)]
      leg.map = NULL
      for (p in 1:nrow(X)) leg.map[p] = paste("cluster", 
                                              X$clust[p], " ", sep = " ")
      Y = cbind.data.frame(Y, as.factor(leg.map))
      res2 = PCA(Y, quali.sup = ncol(Y), scale.unit = FALSE, 
                 row.w = res$call$t$res$call$row.w, ncp = Inf, 
                 graph = FALSE)
      res2$eig <- res$call$t$res$eig
      if (ind.names) 
        plot.PCA(res2, title = title, habillage = ncol(Y), 
                 cex = 0.8, axes = axes, new.plot = new.plot, 
                 palette = palette(clust.col), ...)
      else plot.PCA(res2, title = title, habillage = ncol(Y), 
                    cex = 0.8, axes = axes, label = "none", new.plot = new.plot, 
                    palette = palette(clust.col), ...)
      if (draw.tree) 
        f.draw.tree(X, merge = res$call$t$tree$merge, 
                    height = res$call$t$tree$height, dimens = 2, 
                    t.level = t.level, axes = axes, ...)
    }
  }
  if (choice == "tree" & tree.barplot) 
    par(def.par)
  invisible()
}
