;;; Mode for SIMION geometry files

(defconst gem-other-words '("pa_define" "include" "stl" "inf"))
(defconst gem-electrode-words '("electrode" "non_electrode" "pole" "non_pole" "e" "n"))
(defconst gem-fill-words '("fill" "edge_fill" "rotate_fill" "rotate_edge_fill"))
(defconst gem-within-words
  '("within"
    "within_inside"
    "within_inside_or_on"
    "notin"
    "notin_inside"
    "notin_inside_or_on"
    "intersect"))
(defconst gem-shape-words
  '("box"
    "centered_box"
    "corner_box"
    "box3d"
    "centered_box3d"
    "corner_box3d"
    "circle"
    "cylinder"
    "sphere"
    "parabola"
    "hyperbola"
    "points"
    "points3d"
    "polyline"
    "cylinder3d"
    "half_space"
    "shape"))
(defconst gem-locate-words
  '("locate"
    "project"
    "extrude_xy"
    "extrude_yz"
    "extrude_zx"
    "revolve_xy"
    "revolve_yz"
    "revolve_zx"
    "rotate_x"
    "rotate_y"
    "rotate_z"))

(defconst gem-builtins
  (append gem-other-words
          gem-electrode-words
          gem-fill-words
          gem-within-words
          gem-shape-words
          gem-locate-words))

;;;###autoload
(define-derived-mode gem-mode lua-mode "SIMION gem"
  "Edit SIMION's .gem files"
  (font-lock-add-keywords 'gem-mode
               `((,(regexp-opt gem-builtins 'symbols) . font-lock-builtin-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gem\\'" . gem-mode))

(provide 'wec-gem)
