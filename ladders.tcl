# This provides a graphical/interactive solution to the ladders problem.
#
# Two right-angled triangles T1=ABC and T2=ABD share a horizontal base AB.
# In T1 C is vertically above B and in T2 D is vertically above A such that AC=4m and BD=5m.
# AC and BD intersect at E and the point F on AB is such that EF=2m is vertical.
# What is the distance AB to the nearest mm?
#
# Algebraeically is w=AB then w satisfies:
#
#          1                1           1
#    ------------  +  ------------  =   -
#    sqrt(25-w^2)     sqrt(16-w^2)      2
#
# Solving gives w=1.895m

# To use this app start it with with either tclsh or wish
# Use Button-1 motion to move the geometry until the label on the grey vertical line is 2.000
# then the label on the bottom line will be the solution.

###############################################################################

package require Tk

namespace eval GUI {
    variable win
    variable c
    variable aspect
    variable xoff 60
    variable yoff 30
    variable dragStart {0 0}
    variable currentParameter 0
    
    # Create a window with the given screen that maps onto the
    # coordinates for the required problem
    proc create {screenWin coordWin} {
        variable aspect
        variable c
        variable win
        
        lassign $screenWin w h
        array set win [list w $w h $h]
        set mf [ttk::frame .fr -w $win(w) -h $win(h)]
        set c [canvas $mf.c -width $win(w) -height $win(h) -bg #FFFFAA]
        pack $c $mf -fill both -expand 1
        
        # Set the transformation from coordWin to screenWin
        lassign $coordWin x y
        set xaspect [expr {$w/$x}]
        set yaspect [expr {$h/$y}]
        if {$xaspect < $yaspect} {
            set aspect $xaspect
        } else {
            set aspect $yaspect
        }
        
        bind $c <ButtonPress-1> {
            set GUI::dragStart [list [%W canvasx %x] [%W canvasy %y]]
        }
        
        # Implement drag on button-1 motion.
        # This is an edit operation - not a delete & recreate operation.
        bind $c <B1-Motion> {
            
            # Calculate the delta (relative to the drag start)
            set p [list [%W canvasx %x] [%W canvasy %y]]
            set canvas_delta {0 0}
            for {set i 0} {$i < 2} {incr i} {
                lset canvas_delta $i [expr {[lindex $p $i]-[lindex $GUI::dragStart $i]}]
            }
            puts $canvas_delta
            
            # Convert the delta to problem coords
            set prob_delta [GUI::invMapCoords $canvas_delta]
                
            # Get the new problem coordinates corresponding to the parameter created by the delta
            puts "[GUI::getParameter $prob_delta] ; $GUI::currentParameter"
            set t [expr {[GUI::getParameter $prob_delta] + $GUI::currentParameter}]

            foreach dI $Lines::rlines {
                if {[catch {eval $dI $t} new_coords]} {
                    puts $new_coords
                    return -code break
                }
    
                # Map back to the canvas
                set new_from [GUI::mapCoords [dict get $new_coords from]]
                set new_to [GUI::mapCoords [dict get $new_coords to]]
                
                # Update the line's coordinates on the canvas
                %W coords $dI {*}[concat $new_from $new_to]
                
                if {[%W gettags "label-$dI"] eq "label-$dI"} {
                    set nlab [GUI::getLabel $dI $t]
                    %W dchars "label-$dI" 0 end
                    %W insert "label-$dI" 0 $nlab
                    set mid [GUI::getTextOrigin %W $dI $t]
                    %W coords "label-$dI" {*}$mid
                }
                
            }
        }
    }
    
    proc getTextOrigin {c f t} {
        set box [$c bbox "label-$f"]
        set bh_width [expr {([lindex $box 2]-[lindex $box 0])/2}]
        set bh_height [expr {([lindex $box 3]-[lindex $box 1])/2}]
        set tp [Lines::getTextProperties $f]
        lassign [GUI::mapCoords [Lines::parameterToPoint $f $t [lindex $tp 1]]] mx my
        # Adjust coords for the text origin
        set xo [lindex $tp 0 0]
        set yo [lindex $tp 0 1]
        return [list [expr {$mx-$xo*$bh_width}] [expr {$my+$yo*$bh_height}]]
    }
    
    proc getLabel {f t} {
        set llen [Lines::getLength $f $t]
        set nlab [format "%.3f" "$llen"]
        return $nlab
    }
    
    proc createLine {from to {f line}} {
        variable c
        variable win
        set props [Lines::getLineProperties $f]
        $c create line {*}[mapCoords $from] {*}[mapCoords $to] -tag $f {*}$props
    }
    
    # Map problem coords to canvas coords
    proc mapCoords {p} {
        variable win
        variable aspect
        variable xoff
        variable yoff
        lassign $p x y
        return [list [expr {$aspect*$x+$xoff}] [expr {$win(h)-$yoff-$aspect*$y}]]
    }
    
    # Map canvas coords to problem coords (inverse of mapCoords)
    proc invMapCoords {p} {
        variable win
        variable aspect
        variable xoff
        variable yoff
        lassign $p x y
        return [list [expr {($x-$xoff)/$aspect}] [expr {($win(h)-$y-$yoff)/$aspect}]]
    }
    
    # Create a constrained line defined by the function f 
    proc createConstrainedLine {f t} {
        variable c
        variable currentParameter

        set p [eval $f $t]
        GUI::createLine [dict get $p from] [dict get $p to] $f
        if {[Lines::getLabelState $f]} {
            set mid [GUI::mapCoords [Lines::parameterToPoint $f $t 0.5]]
            set txt [getLabel $f $t]
            $c create text {*}$mid -text $txt -tag "label-$f"
            set mid [GUI::getTextOrigin $c $f $t]
            $c coords "label-$f" {*}$mid
        }
    }
        
    proc getParameter {delta} {
        lassign $delta dt
        return [expr {$dt}]
    }
}

namespace eval Lines {
    variable rlines {}
    variable properties
    variable defaultLineProps {-fill blue -width 1}
    variable defaultTextProps [dict create state off origin {0 0} param 0.5]
    variable labels 
    variable texts
    
    # Register a line
    proc register {l} {
        variable rlines
        if {[lsearch $rlines $l] == -1} {
            lappend rlines $l
        }
    }
    
    # Set defaults
    proc setDefaultProperties {props} {
        variable defaultLineProps
        variable defaultLabelProps
        set defaultLineProps [dict get $props line]
        set defaultLabelProps [dict get $props label]
    }
    
    # Set properties for a line
    proc setProperties {l p} {
        variable properties
        variable rlines
        if {[lsearch $rlines $l] == -1} {
            error "$l is not registered"
        } else {
            set properties($l) $p
        }
    }
    
    # Set labelling
    proc setLabels {l s} {
        variable rlines
        variable labels
        if {[lsearch $rlines $l] == -1} {
            error "$l is not registered"
        } else {
            if {$s == {on}} {
                set labels($l) 1
            } else {
                set labels($l) 0    
            }
        }
    }
    
    # Get required label state
    proc getLabelState {l} {
        variable rlines
        variable labels
        if {[lsearch $rlines $l] == -1} {
            error "$l is not registered"
        } else {
            if {! [info exists labels($l)]} {
                return 0 ;# default is off
            } else {
                return $labels($l)
            }
        }
    }
    
    # Retrieve a line's properties
    proc getLineProperties {l} {
        variable rlines
        variable properties
        variable defaultLineProps
        if {[info exists properties($l)]} {
            return [dict get $properties($l) line]
        } else {
            return $defaultLineProps
        }
    }
    
    # Retrieve a line's text properties
    proc getTextProperties {l} {
        variable rlines
        variable properties
        variable defaultLabelProps
        if {[info exists properties($l)]} {
            return [dict get $properties($l) label]
        } else {
            return $defaultLabelProps
        }
    }
    
    # Get the lines length
    proc getLength {l t} {
        lassign [getEndPoints $l $t] p1 p2
        set tv [Geometry::createVector $p1 $p2]
        return [expr {hypot([lindex $tv 0],[lindex $tv 1])}]
    }
    
    # Create all lines
    proc createAll {t} {
        variable rlines
        set GUI::currentParameter $t
        foreach l $rlines {
            GUI::createConstrainedLine $l $t           
        }
    }
    
    # Calculate the intersection of two lines (assummed to be non-parallel)
    proc intersect {l1 l2 t} {
        variable rlines
        if {[lsearch $rlines $l1] == -1} {
            error "$l1 is not registered"
        }
        if {[lsearch $rlines $l2] == -1} {
            error "$l2 is not registered"
        }
        
        # End-points of the two lines
        lassign [getEndPoints $l1 $t] p1 p2
        lassign [getEndPoints $l2 $t] q1 q2
        
        # Quick access to the Geometry code
        namespace path ::Geometry
        
        # Create a vector in the direction of the first line
        set p_t [createVector $p1 $p2]
        
        #  Create a vector perpendicular to the second line
        set q_n [createNormal $q1 $q2]
        
        # Calculate the parameter of the intersection point along the first line
        set u [expr {[dotProduct [createVector $p1 $q1] $q_n]/[dotProduct $p_t $q_n]}]
        
        # Convert the parameter to a point
        return [pointFromParameter $p1 $p_t $u]
    }
    
    proc getEndPoints {l t} {
        set p_d [eval $l $t]
        return [list [dict get $p_d from] [dict get $p_d to]]
    }
    
    # Return a point along a line: u = 0 => start; u = 1 => end
    proc parameterToPoint {l t u} {
        lassign [getEndPoints $l $t] p1 p2
        set r {0 0}
        for {set i 0} {$i < 2} {incr i} {
            lset r $i [expr {[lindex $p1 $i]+$u*([lindex $p2 $i]-[lindex $p1 $i])}]
        }
        return $r
    }
}

namespace eval Geometry {
        proc createVector {p1 p2} {
            set r {0 0}
            for {set i 0} {$i < 2} {incr i} {
                lset r $i [expr {[lindex $p2 $i]-[lindex $p1 $i]}]
            }
            return $r
        }
        
        proc createNormal {p1 p2} {
            set t [createVector $p1 $p2]
            return [list [expr {-[lindex $t 1]}] [lindex $t 0]]
        }
        
        proc dotProduct {v1 v2} {
            return [expr {[lindex $v1 0]*[lindex $v2 0]+[lindex $v1 1]*[lindex $v2 1]}]
        }
        
        proc pointFromParameter {p t u} {
            set r {0 0}
            for {set i 0} {$i < 2} {incr i} {
                lset r $i [expr {[lindex $p $i]+$u*[lindex $t $i]}]
            }
            return $r
        }
    }

proc AC {t} {
    if {0 < $t && $t <  4} {
        set from {0 0}
        set to [list [expr {$t}] [expr {sqrt(16-$t*$t)}]]
        return [dict create from $from to $to]
    } else {
        error "Parameter out-of-range"
    }
}

proc BD {t} {
    if {0 < $t && $t < 5} {
        set to [list 0 [expr {sqrt(25-$t*$t)}]]
        set from [list [expr {$t}] 0]
        return [dict create from $from to $to]
    } else {
        error "Parameter out-of-range"
    }
}

proc BC {t} {
    set from [dict get [BD $t] from]
    set to [dict get [AC $t] to]
    return [dict create from $from to $to]
}

proc AD {t} {
    set from [dict get [AC $t] from]
    set to [dict get [BD $t] to]
    return [dict create from $from to $to]
}

proc AB {t} {
    set from [dict get [AC $t] from]
    set to [dict get [BD $t] from]
    return [dict create from $from to $to]
}

proc EF {t} {
    set from [Lines::intersect AC BD $t]
    set to [list [lindex $from 0] 0]
    return [dict create from $from to $to]
}

# Register all lines so that they will dynamically update
Lines::register AC
Lines::register BD
Lines::register BC
Lines::register AD
Lines::register AB
Lines::register EF

# Set the default properties
Lines::setDefaultProperties [dict create line {-fill blue -width 1} label {off {0 0} 0.5}]

# Set the non-default properties
Lines::setProperties EF [dict create line {-fill grey -width 1} label {{-1 0} 0.5}]
Lines::setProperties AB [dict create line {-fill blue -width 2} label {{0 1} 0.5}]
Lines::setProperties AC [dict create line {-fill red -width 2} label {{1 0} 0.9}]
Lines::setProperties BD [dict create line {-fill red -width 2} label {{-1 -1} 0.8}]

# Turn on labelling
Lines::setLabels EF on
Lines::setLabels AB on
Lines::setLabels AC on
Lines::setLabels BD on

# Create the basic canvas
GUI::create {400 400} {6 6}

# Create all the registered lines
Lines::createAll 3

                  