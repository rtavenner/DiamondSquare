import DiamondSquare exposing (diamondSquare)
import Array
import Random
import WebGL exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Time exposing (Time)

world =
    Random.initialSeed 1234567 
    |> Random.step (diamondSquare 6 (0,0,0,0))
    |> Tuple.first
    |> Array.map (Array.map ((*) 8))

size = Array.length world

mesh : Mesh Vertex
mesh = WebGL.indexedTriangles
    (world
        |> Array.indexedMap 
            (\x -> Array.indexedMap 
                (\y h -> 
                    { position = 
                        vec3 
                            (toFloat x / toFloat size - 0.5)
                            (toFloat y / toFloat size - 0.5)
                            (max h -1.01 / 7)
                    , color =
                        if h < -1
                        then vec3 0 0 (clamp 0 0.5 (0.5*h + 1))
                        else if h < -1/2
                        then vec3 1 1 0.75
                        else vec3 0 (clamp 0 1 (2/3-h/3)) 0
                    }))
        |> Array.toList
        |> List.map Array.toList
        |> List.concat)
    (List.concatMap
        (\x -> List.concatMap
            (\y ->
                let n = size*x + y
                in 
                    [ (n,n+1,n+size+1)
                    , (n+size+1,n+size,n)
                    ])
            (List.range 0 (size-2)))
        (List.range 0 (size-2)))


-- Everything below from webgl cube example

type alias Model = Float
type alias Msg = Time

main : Program Never Model Msg
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions = (\_ -> AnimationFrame.diffs Basics.identity)
        , update = (\dt theta -> ( theta + Time.inSeconds dt / 3, Cmd.none ))
        }


view : Model -> Html Msg
view theta =
    WebGL.toHtml
        [ width 1200
        , height 900
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms theta)
        ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Model -> Uniforms
uniforms theta =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (-pi/2) (vec3 1 0 0))
            (Mat4.makeRotate (theta) (vec3 0 0 1))
    , perspective = Mat4.makePerspective 45 (4/3) 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 1.5 1) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
