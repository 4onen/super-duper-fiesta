module Graphics exposing (..)

import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest
import Math.Vector2 as V2 exposing (Vec2,vec2)
import Math.Vector3 as V3 exposing (Vec3,vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

entitySettings =
    [ WebGL.Settings.cullFace WebGL.Settings.back
    , WebGL.Settings.DepthTest.default
    ]

type alias Attributes =
    { position: Vec3
    , normal: Vec3
    , texcoord: Vec2
    }

type alias Uniforms = 
    { worldToClip: Mat4
    , transform: Mat4
    , normalTransform: Mat4
    , recolor: Vec3
    , time: Float
    , lightDir: Vec3
    }

initUniforms recol time lightDir = 
    Uniforms
        Mat4.identity
        Mat4.identity
        Mat4.identity
        recol
        time
        lightDir

simpleVertex = [glsl|
precision mediump float;

uniform mat4 worldToClip;
uniform mat4 transform;
uniform mat4 normalTransform;

attribute vec3 position;
attribute vec3 normal;
attribute vec2 texcoord;

varying vec3 p;
varying vec3 n;
varying vec2 tc;

void main(){
    vec4 pos = transform*vec4(position,1.0);
    p = pos.xyz;
    n = normalize((normalTransform*vec4(normal,1.0)).xyz);
    tc = texcoord;
    gl_Position = worldToClip*pos;
}
|]

platformFragment = [glsl|
precision mediump float;

uniform vec3 recolor;
uniform vec3 lightDir;

varying vec3 p;
varying vec3 n;
varying vec2 tc;

//Noise
float rand(vec2 p){
    return fract(sin(dot(p.xy,vec2(12.9898,78.233)))*43758.5453123);
}

float noise2d(vec2 p){
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 d = vec2(1,0);

    return mix(mix(rand(i),rand(i+d.xy),f.x),mix(rand(i+d.yx),rand(i+d.xx),f.x),f.y);
}

void main(){
    const float s = 5.0;
    vec2 u = mat2(s,-s,-s,-s)*tc;
    float f = noise2d(u-2.*noise2d(u+4.*noise2d(0.5*u)));
    float sun = clamp(dot(lightDir,vec3(0.0,0.0,1.0)),0.0,3.0);
    vec3 col = recolor*vec3(0.4,0.2,0.05)*sqrt(f);
    col = mix(col*vec3(0.1,0.1,0.3),col,sun);
    gl_FragColor = vec4(sqrt(col),1.0);
}
|]

platformMesh : WebGL.Mesh Attributes
platformMesh = 
    let
        triCount = 32
    in
        List.range 0 triCount
            |> List.map 
                (\i -> 
                    vec3 
                        (sin (-2.0*Basics.pi*(toFloat i)/(toFloat triCount)))
                        (cos (-2.0*Basics.pi*(toFloat i)/(toFloat triCount)))
                        (0.0)
                )
            |> (::) (vec3 0.0 0.0 0.0)
            |> List.map 
                (\p -> 
                    Attributes
                        (p) 
                        (V3.normalize <| V3.add p (vec3 0.0 0.0 0.1))
                        (vec2 (V3.getX p) (V3.getY p))
                )
            |> WebGL.triangleFan

platform = 
    WebGL.entityWith
        entitySettings
        simpleVertex
        platformFragment
        platformMesh

oceanVertex = [glsl|
precision mediump float;

uniform mat4 worldToClip;
uniform mat4 transform;

attribute vec3 position;
attribute vec2 texcoord;

varying vec3 p;
varying vec3 n;
varying vec2 tc;

void main(){
    vec4 p1 = transform*vec4(position,1.0);
    p = vec3(1000.0*p1.xy,p1.z);
    n = vec3(0.0,0.0,1.0);
    tc = texcoord;
    vec4 o = worldToClip*vec4(1000.0*p1.xy,p1.z,1.0);
    gl_Position = vec4(2.*o.xy,o.z,o.w);
}|]

oceanFragment = [glsl|
precision mediump float;

uniform float time;
uniform vec3 lightDir;

varying vec3 p;
varying vec3 n;
varying vec2 tc;

//Noise
float rand(vec2 p){
    return fract(sin(dot(p.xy,vec2(12.9898,78.233)))*43758.5453123);
}

float noise2d(vec2 p){
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 d = vec2(1,0);

    return mix(mix(rand(i),rand(i+d.xy),f.x),mix(rand(i+d.yx),rand(i+d.xx),f.x),f.y);
}

void main(){
    vec2 u = 1000.0*tc+time;
    
    float f = noise2d(u-2.*noise2d(u));

    float sun = clamp(dot(lightDir,n),0.0,1.0);
    vec3 col = mix(vec3(0.392,0.489,0.509),vec3(0.49,0.28,0.50),sqrt(f));
    col = mix(col*vec3(0.3,0.3,0.5),col,sun);

    gl_FragColor = vec4(col,1.0);
}|]

oceanMesh =
    WebGL.indexedTriangles
        (List.map (\p -> Attributes p (vec3 0.0 0.0 1.0) (vec2 (V3.getX p) (V3.getY p)))
            [vec3 -1.0 -1.0 0.0
            ,vec3 1.0 -1.0 0.0
            ,vec3 1.0 1.0 0.0
            ,vec3 -1.0 1.0 0.0
            ]
        )
        [(0,1,3),(3,1,2)]

ocean =
    WebGL.entityWith
        entitySettings
        oceanVertex
        oceanFragment
        oceanMesh

topFragment = [glsl|
precision mediump float;

uniform vec3 recolor;
uniform vec3 lightDir;

varying vec3 p;
varying vec3 n;
varying vec2 tc;

void main(){
    float sun = clamp(dot(n,vec3(0.0,0.0,1.0)),0.0,1.0)*clamp(dot(n+vec3(0.0,0.0,1.0),lightDir),0.0,1.0);

    vec3 col = recolor*(0.5*n+1.0);
    col = mix(col*vec3(0.3,0.3,0.5),col,sun);

    gl_FragColor = vec4(sqrt(col),1.0);
}
|]

topMesh =
    let
        res_phi = 3
        res_theta = 5
        vertices = 
            List.range 0 (res_theta-1)
                |> List.map (\i -> List.range 0 (res_phi-1) |> List.map (\j -> (i,j)))
                |> List.concat
                |> List.map
                    (\(i,j) -> 
                        let
                            tcx = (toFloat i)/(res_theta-1)
                            tcy = (toFloat j)/(res_phi-1)
                            theta = 2*Basics.pi*tcx
                            phi = Basics.pi*tcy
                            p = (vec3 ((cos theta) * (sin phi)) ((sin theta)*(sin phi)) (cos phi))
                        in
                            Attributes
                                p
                                p
                                (vec2 tcx tcy)
                    )
        indices =
            List.range 0 (res_theta-2)
                |> List.map (\i -> List.range 0 (res_phi-2) |> List.map 
                    (\j -> 
                        [ (i*res_phi+j,i*res_phi+j+1,(i+1)*res_phi+j+1)
                        , (i*res_phi+j,(i+1)*res_phi+j+1,(i+1)*res_phi+j)
                        ]
                    ))
                |> List.concat
                |> List.concat
    in
        WebGL.indexedTriangles vertices indices


top =
    WebGL.entityWith
        entitySettings
        simpleVertex
        topFragment
        topMesh
