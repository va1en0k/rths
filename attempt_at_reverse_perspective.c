
vec3 cameraPos = vec3(0.0, 0.0, -2.0);

vec4 lineIntersect(vec3 from, vec3 to, vec2 bounds, vec3 rayFromCamera) {

    vec3 o = from - cameraPos;
    // t1 = (D2*t2 - o)/D1 = kt2 + b;

    vec3 k = rayFromCamera / to;
    vec3 b = o / to;

    // x(kt2 + b) = y(kt2 + b)

    float t2 = (k.x - k.z) / (b.x - b.z);
    //if (abs(b.x - b.z) <= 8.0) return vec4(1.0, 0.0, 0.0, 1.0);
	//if (t2xy <= 0.0) return vec4(vec3(0.0), 1.0);

//    float t2xz = (k.x - k.z) / (b.y - b.z);

    float t1 = t2 * k.x + b.x;

    //if (t1 < bounds.x || t1 > bounds.y) return vec4(vec3(0.0), 1.0);

   // if (distance(from + t1.x * to, from + t1.y * to) > 0.1) return vec4(vec3(0.0), 1.0);
	float p = abs(k.y * t2 + b.y - t1);
   // return vec4(t2 / 10.0 + 0.3);
    if (p >= 0.01) {
        return vec4(0.0);
    }

    return vec4(1.0);
}

vec4 sphere(vec3 p, float r, vec3 rayFromCamera) {
  vec3 oc = cameraPos - p;
  float a = dot(rayFromCamera, rayFromCamera);
  float b = 2.0 * dot(oc, rayFromCamera);
  float c = dot(oc, oc) - r * r;
  float dsc = b * b - 4.0 * a * c;
  if (dsc < 0.0) return vec4(0.0);
  return vec4(1.0);
}



vec4 point(vec3 p, vec3 rayFromCamera) {
	return sphere(p, 0.02, rayFromCamera);
}

vec4 table(vec3 a, vec3 b, vec3 c, vec3 d, vec3 r) {
	vec4 col = lineIntersect(a, b, vec2(0.0, 1.0), r);
    col += lineIntersect(b, c, vec2(0.0, 1.0), r);
    col += lineIntersect(c, d, vec2(0.0, 1.0), r);
    col += lineIntersect(d, a, vec2(0.0, 1.0), r);
    return col;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord/max(iResolution.x, iResolution.y);
    vec2 vpSize = iResolution.xy / max(iResolution.x, iResolution.y);

    // Time varying pixel color
    //vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));
    //

    vec3 cameraRay = vec3(2.0 * (uv - vpSize/2.0), 0) - cameraPos;

    //fragColor = lineIntersect(vec3(-1.0, 0.1, 1.0),
  //                            vec3(1.0, 0.1, 1.0), vec2(-0.9, -0.3), cameraRay);

    fragColor = vec4(0.0);
    //if (fragColor.a < 0.1) {
    fragColor += table(
        vec3(-1.0, 0.1, 1.0),
        vec3(1.0, 0.1, 1.0),
        vec3(1.0, 0.1, 2.0),
        vec3(-1.0, 0.1, 2.0),
        cameraRay
    );

    //fragColor += lineIntersect(vec3(1.0, 0.1, 1.0),
    //                           vec3(-1.0, 0.1, 1.0), vec2(0.0, 1.0), cameraRay);

    fragColor += point(vec3(-1.0, 0.0, 1.0), cameraRay);
    fragColor += point(vec3(1.0, 0.0, 1.0), cameraRay);
    //}

    // Output to screen
    //fragColor = vec4(col,1.0);
}
